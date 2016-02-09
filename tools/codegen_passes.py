#!/usr/bin/env python3

import time
import os
import sys
import xml.etree.ElementTree as et
import subprocess

# configuration variables

""" Root directory where the doxygen files are found.
"""
doxygenRoot = ""
destDir = ""
force = False
verbose = False
cppRoot = ""
clangFormat = ""


""" Dictionary for all classes loaded from the documentation.

Gives class names to their class objects. This is a shortcut if we only have a class name and are not sure where to look (notably we are not sure if we are dealing with properties or patterns). There are of course more optimal ways to do this (i.e. linking classes to each other instead of using names as we do now, but that leads to more complex code).
"""
classes = {}
""" Dictionary of all Pattern classes that were extracted from the documentation.

These are extracted by taking the PATTERN_BASE class and loading all its children.
"""
PATTERN_BASE = "rjit::ir::Pattern"
patterns = {}
""" Dictionary of all property classes that were extracted from the documentation.

A class is considered a property class if there is at least one pattern class that inherits from it and the class itself is not a pattern.
"""
properties = {}
""" Set of all llvm instructions that were extracted from the documentation.

We do not scan llvm documentation to obtain the instructions, but rather assume that any argument to a match method that is not pattern, property or predicate must be llvm instruction. Instruction names stored here include the llvm:: prefix.
"""
instructions = set()
""" Dictionary of all predicates used for the matching.

A predicate is any child of the PREDICATE_BASE class.
"""
PREDICATE_BASE = "rjit::ir::Predicate"
predicates = {}
""" Dictionary of all passes for which their dispatch methods must be generated.

A pass is any class that inherits from PASS_BASE class.
"""
PASS_BASE = "rjit::ir::Pass"
passes = {}


def help():
    print("""
    Code generation for pass dispatchers and pattern kinds. Usage:

    codegen_passes.py doxygen_root dest_dir clang_format [force] [verbose]

    doxygen_root is path to doxygen xml files which document the ir patterns and passes.

    dest_dir is directory to which the generated cpp files should be put.

    clang_format is path to proper clang_format executable for beautification of the generated code. If this is not required, use noformat as the value.

    if force is specified all files will be regenerated even if the script would think it is not needed

    if verbose is specified, a lot of mostly useless stuff will be printed.
    """)

def error(file, line, message, fail=True):
    """ Displays an error with proper formatting and exits the application
    immediately, unless fail == False.
    """
    print("{file}:{line}: error: {message}".format(
        file=file, line=line, message=message), file=sys.stderr)
    if (fail):
        sys.exit(-1)

def D(message):
    if (verbose):
        print(message)


def typeFromXML(xml):
    if (xml.text):
        # it isundocumented type (primitive, ...)
        result = xml.text
        # strip pointer or reference marks
        if (result[-2:] in  (" *", " &")):
            result = result[:-2]
        return result
    elif (len(xml) == 0):
        # it is constructor or destructor return type, i.e. no type
        return ""
    else:
        # it is a reference to known type, obtain the type name from the reference
        refid = xml[0].attrib["refid"] + ".xml"
        if (os.path.isfile(os.path.join(doxygenRoot, refid))):
            # it is a known class, get the class name from the
            return CppClass.demangle(refid)
        else:
            return xml[0].text

# ---------------------------------------------------------------------------------------------------------------------




class DispatchRecord:
    """ A dispatch record contains information about the unconditional matcher (there can be only one per record) and conditional matchers (more conditional matchers are possible, they will be tested in the order they appear in the source code), as well as next dispatch table, in case that matchers match sequence of patterns / properties / instructions.
    """
    def __init__(self):
        self.nextPatterns = {}
        self.nextLLVM = {}
        self.unconditional = False
        self.conditional = []

    def arguments(self):
        """ Returns the arguments string for calling predicates and match methods. Contains typecasted instructions, or patterns/properties as they should appear in the invocations. """
        # first determine where to get the argument types from
        if (self.unconditional):
            args = self.unconditional.args
        elif (self.conditional):
            args = self.conditional[0].args[1:]
        else:
            return False
        # now for each argument, if it is in llvm Instructions, it is llvm instruction, and we will use itertor, otherwise we use pattern
        result = []
        for i in range(len(args)):
            if (args[i] in instructions):
                result.append("static_cast<{iType} *>(&*it{i})".format(iType = args[i], i = i))
            else:
                result.append("static_cast<{pType} *>(p{i})".format(pType = args[i], i = i))
        return ", ".join(result)


    def emit(self, f, current):
        """ Emits code for the dispatch record.

        F is the file to write the result to, current is effectively the name of the iterator that holds the instruction to be matched.
        """
        # iterator to instruction that we are going to match
        it = "it{0}".format(current)
        # iterator to the instruction after the one we try to match
        itNext = "it{0}".format(current + 1)
        # create the next iterator if we are going to match
        if (self.nextPatterns or self.nextLLVM):
            print("llvm::BasicBlock::iterator {itNext} = {it};".format(itNext = itNext, it = it), file = f)
        # check the patterns first
        if (self.nextPatterns):
            p = "p{0}".format(current)
            print("""
            Pattern * {p} = Pattern::get({it});
            if ({p} != nullptr)
                switch ({p}->kind) {{""".format(p = p, it = it), file = f)
            # now switch on the arguments
            for pName, dr in self.nextPatterns.items():
                print("""
                case Pattern::Kind::{pName}: {{
                    {p}->advance({itNext});""".format(pName = pName.split("::")[-1], p = p, itNext = itNext), file = f)
                dr.emit(f, current + 1)
                print("break; }", file = f)
            print("}", file = f)
        # now llvm instructions matching
        if (self.nextLLVM):
            # next iterator will just be next instruction
            print("++{itNext};".format(itNext = itNext), file = f);
            for iName, dr in self.nextLLVM.items():
                print("if (llvm::isa<{iName}>({it})) {{".format(iName = iName, it = it), file = f)
                dr.emit(f, current + 1)
                print("}", file = f)
        # create the sequence of iterators for the calls, we know there has to be at least one
        args = self.arguments()
        # finally, check if we have any predicated match functions
        for mm in self.conditional:
            print("""{{
            {pType} p = {pType}();
            if (p.match(*this, {args})) {{
                {mm}({args}, p);
                it0 = {it};
                return true;
            }} }}""".format(pType = mm.predicate, args = args, mm = mm.name, it = it), file = f)
        # finally, if we have nonpredicated match, execute it
        if (self.unconditional):
            print("""
            {mm}({args});
            it0 = {it};
            return true;
            """.format(mm = self.unconditional.name, args = args, it = it), file = f)

    def _addMatch(self, matcher, index):
        # first see if there is anything left in the match sequence, and if not, add the matcher method to the dispatch record directly.
        seq = matcher.matchSequence
        if (index == len(seq)):
            if (not matcher.predicate):
                if (self.unconditional):
                   if (self.unconditional.matchsetSize() == matcher.matchsetSize()):
                        error(matcher.file, matcher.line, "Dispatch table ambiguity - collision with match method at {0}, {1}".format(self.unconditional.file, self.unconditional.line))
                    # don't do anything if new matcher is more general than existing one
                   if (matcher.matchsetSize() > self.unconditional.matchsetSize()):
                       return
                   # otherwise overwrite the existing one with the more specific
                # or just write if not written yet
                self.unconditional = matcher
            else:
                # conditional matchers are always fine, we can have as many of them as we like.
                self.conditional.append(matcher)
        else:
            # get type we are going to match now
            t = seq[index]
            if (t in instructions):
                # llvm instructions are simpler, there is always only the instrcuction in the matchset, see if we have a dispatch record for it and create one if not
                if (t not in self.nextLLVM):
                    self.nextLLVM[t] = DispatchRecord()
                # add the matcher and remaining sequence to the record
                self.nextLLVM[t]._addMatch(matcher, index + 1)
            else:
                # t must be either a pattern or a property, and we have to deal with matchsets, first obtain the class object rather than the type
                if (t in patterns):
                    t = patterns[t]
                else:
                    t = properties[t]
                # now for each pattern in t's matchset, do the addition
                for tt in t.matchset:
                    if (tt not in self.nextPatterns):
                        self.nextPatterns[tt] = DispatchRecord()
                    self.nextPatterns[tt]._addMatch(matcher, index + 1)

# ---------------------------------------------------------------------------------------------------------------------

class CppMethod:
    """ Information about a C++ method from doxygen files. """

    def __init__(self, xml):
        """ Creates the method from given xml. """
        self.xml = xml
        self.args = []
        self.type = ""
        for child in xml:
            if (child.tag == "name"):
                self.name = child.text
            elif (child.tag == "type"):
                self.type = typeFromXML(child)
            elif (child.tag == "location"):
                self.file = child.get("file")
                self.line = child.get("line")
            elif (child.tag == "param"):
                for x in child:
                    if (x.tag == "type"):
                        self.args.append(typeFromXML(x));

    def isMatchMethod(self):
        """ Returns true if given method is a match method.

        Match method should have reported return type of match, where match is a void typecast defined in rjit::ir::Pass.
        """
        return self.type == "match"

    def matchsetSize(self):
        """ Returns the matchset size, assuming the method is unconditional matcher. """
        x = self.args[-1]
        if (x in patterns):
            return len(patterns[x].matchset)
        if (x in properties):
            return len(properties[x].matchset)
        return 1

    def checkMatchMethod(self):
        """ Checks that given method conforms to the match method signature.

        A match method has at least one instruction, pattern or property argument and may have at most one predicate as last argument.

        Also sets the predicate field with the typename of the predicate, or "" if no predicate is used and finally initializes the matchSequence list containing all predicates, properties and instructiosn the method handles.
        """
        if (not self.args):
            error("Matcher method {0} expected to have at least one argument.".format(self.name))
        self.predicate = ""
        self.matchSequence = []
        for i in range(len(self.args)):
            name = self.args[i]
            if (name in predicates):
                if (self.predicate):
                    error(self.file, self.line, "Matcher method {0} can have only one predicate.".format(self.name))
                if (i != len(self.args) - 1):
                    error(self.file, self.line, "Matcher method {0} predicate must be last argument.".format(self.name))
                self.predicate = name
            else:
                # we know it has to be pattern, property, or instruction because instruction is anything but pattern & property
                self.matchSequence.append(name)
        if (not self.matchSequence):
            error(self.file, self.line, "Matcher method {0} does not match any instructions or patterns".format(self.name))

# ---------------------------------------------------------------------------------------------------------------------

class CppClass:

    @staticmethod
    def demangle(filename):
        """ Returns the class name corresponding to the given filename. Does not expect the filename to contain the doxygenRoot path prefix. """
        # get rid of leading class and trailing .xml
        name = filename[5:-4]
        result = ""
        i = 0
        while (i < len(name)):
            if (name[i] == "_"):
                i += 1
                if (name[i] == "1"):
                    result += ":"
                else:
                    result += name[i].upper()
            else:
                result = result + name[i]
            i += 1
        return result

    @staticmethod
    def doxygenFilename(className):
        """ Returns the class xml filename under which the specified class' documentation should be found.

        The manging changes :: to _1_1 and capital leters to _lowercase.
        """
        result = os.path.join(doxygenRoot, "class")
        for c in className:
            if (c == ":"):
                result += "_1"
            elif (c.isupper()):
                result += "_" + c.lower()
            elif (c == "_"):
                result += "__"
            else:
                result += c
        return result+".xml"


    def __init__(self, name):
        """ Initializes the cpp class and loads the xml documentation. """
        global classes
        classes[name] = self
        self.name = name
        self.filename = CppClass.doxygenFilename(name)
        self.subclasses = []
        self.parentClasses = []
        if (not os.path.isfile(self.filename)):
            D("Documentation for class {0} not found (expected filename {1})".format(name, self.filename))
            self._xml = False
        else:
            self._xml = et.parse(self.filename).getroot()[0]
            # get the subclasses
            for child in self._xml:
                if (child.tag == "derivedcompoundref"):
                    self.subclasses.append(child.text)
                elif (child.tag == "basecompoundref"):
                    self.parentClasses.append(child.text)
                elif (child.tag == "location"):
                    self.file = child.get("file")
                    self.line = child.get("line")

    def includePath(self):
        """ Returns the include path of this file, i.e. what should go in the include statement.Some versions of doxygen don't generate absolute path, in that case rjit/src just need to be removed from the path."""
        return self.file[len(cppRoot) + 1:] if cppRoot in self.file else self.file[9:]

    def isLeaf(self):
        """ Returns True if the class has no subclasses. """
        return not self.subclasses

    def addToMatchset(self, name):
        """ Appends given class to the matchset and creates the matchset if it does not yet exist for the class. """
        if (not hasattr(self, "matchset")):
            self.matchset = set()
        self.matchset.add(name)
        for p in self.parentClasses:
            classes[p].addToMatchset(name)

    def extractMatchMethods(self):
        self.matchMethods = []
        D("  pass {0}".format(self.name))
        for child in self._xml:
            if (child.tag == "sectiondef"):
                if (child.get("kind") in ("public-func", "private-func", "protected-func")):
                    for method in child:
                        m = CppMethod(method)
                        if (m.isMatchMethod()):
                            self.matchMethods.append(m)
                            D("    {0}".format(m.name))

    def checkPredicateAgainstMatchMethod(self, mm):
        """ Assumes class is a predicate and checks that it has the necessary interface to be used in given matcher.

        This means that the predicate must have a method named match which returns bool and has the following arguments:

        First argument is a pass (PASS_BASE), other arguments are the instructions which the match method matches in the same order.
        """
        # if the predicate does not yet have a match method, find it
        if (not hasattr(self, "matchMethod")):
            for child in self._xml:
                if (child.tag == "sectiondef"):
                    if (child.get("kind") in ("public-func", "private-func", "protected-func")):
                        for method in child:
                            m = CppMethod(method)
                            if (m.name == "match" and m.type == "bool" and m.args[0] == PASS_BASE):
                                self.matchMethod = m
                                break
            if (not hasattr(self, "matchMethod")):
                error(self.file, self.line, "Predicate {0} does not have a match method or the method has wrong signature for match method {1} (file: {2}, line {3})".format(self.name, mm.name, mm.file, mm.line))
        # check that the match method of the predicate corresponds to the match method of the pass
        if (len(self.matchMethod.args) != len(mm.args)):
            error(self.file, self.line, "Predicate {0} match method matched different length of instructions than matcher method {1}".format(self.name, mm.name))
        # check that the arguments are of same types
        for i in range(len(mm.args)-1):
            if (self.matchMethod.args[i + 1] != mm.args[i]):
                error(self.file, self.line, "Predicate {0} matches different set of instructions than matcher {1} (index {2}, expected {3}, got {4})".format(self.name, mm.name, i + 1, mm.args[i], self.matchMethod.args[i + 1]))
        # all is fine



    def createDispatchTable(self):
        """ Creates the dispatch table for the pass. Errors if there are any ambiguities found when creating the table.

        The dispatch table is a dicttionary with keys being propeties & patterns and values being the dispatch Records (see above).
        """
        self.dispatchTable = DispatchRecord()
        # for each match method, fill in the appropriate tables
        for m in self.matchMethods:
            self.dispatchTable._addMatch(m, 0)


    def shouldEmit(self, filename):
        """ Returns true if the particular pass should be created.

        This happens either if force is true, the generated file does not exist, or its time is older than the original, or the codegen script.
        """
        global force
        if (force):
            return True
        if (not os.path.isfile(self.file)):
            D("Unable to locate source file {0}".format(self.file))
            return True
        ts = os.path.getmtime(self.file)
        if (not os.path.isfile(filename)):
            return True
        td = os.path.getmtime(filename)
        if (ts > td):
            return True
        ts = os.path.getmtime(sys.argv[0])
        return (ts > td)

    def emit(self):
        """ Emits the dispatcher's code.

        """
        # don't emit the base pass
        if (self.name == PASS_BASE):
            return
        filename = os.path.join(destDir, self.name.replace("::", "_") + ".cpp")
        D("  {0}".format(filename))
        if (not self.shouldEmit(filename)):
            D("    skipping")
            return
        with open(filename, "w") as f:
            # first of all output the headers and general stuff
            print("""
            /* This file has been automatically generated. Do not hand-edit. */
            #include <llvm.h>
            #include "RIntlns.h"
            #include "ir/Ir.h"
            #include "ir/primitive_calls.h"
            #include "{header}"

            #pragma GCC diagnostic ignored "-Wswitch"
            bool {className}::dispatch(llvm::BasicBlock::iterator & it0) {{
            """.format(className = self.name, header = self.includePath()), file = f)
            # emit the dispatch table
            self.dispatchTable.emit(f, 0)
            # if the emitted code get here, the match was not successful, delegate to a parent
            for parent in self.parentClasses:
                if (parent in passes):
                    print("return {parentClass}::dispatch(it0);".format(parentClass = parent), file = f)
                    break
            print("}", file = f)
        # now we must do clang-format on the file so that it does not look too ugly
        if (clangFormat != "noformat"):
            subprocess.call([clangFormat, "-i", "-style=file", ".clang_format", filename])
        #clang-format -i -style=LLVM
# ---------------------------------------------------------------------------------------------------------------------

def loadClassHierarchy(baseClass):
    """ Starts with the baseClass and loads it together with all its subclasses (transitively). Returns a dict containing the name of the class as key and its documentation xml as value. """
    D("Loading class hierarchy for {0}".format(baseClass))
    result = {}
    # load the classes
    q = [ baseClass, ]
    while (q):
        key = q.pop()
        if (not key in result):
            D("  loading class {0}".format(key))
            value = CppClass(key)
            result[key] = value
            # check if the class has any children and add them to the queue
            for child in value.subclasses:
                q.append(child)
    return result

def loadClassParents(child, into):
    """ Recursively loads all parents of the given class into the result dictionary if they are not present yet.
    """
    q = child.parentClasses[:]
    while (q):
        key = q.pop()
        if (not key in result):
            D("  loading class {0}".format(key))
            value = CppClass(key)
            into[key] = value
            for parent in value.parentClasses:
                q.append(parent)

def loadProperties():
    """ Returns a dictionary of property classes inferred from the patterns.

    Note that property is any class that is parent of any pattern that is not child of pattern itself.
    """
    global properties
    D("loading properties")
    for p in patterns.values():
        # get properties of current pattern, i.e. all its parents that are not patterns themselves
        props = [ x for x in p.parentClasses if x not in patterns ]
        # for each property, add it to the result, if it has not been added yet
        for pp in props:
            if (pp not in properties):
                value = CppClass(pp)
                properties[pp] = value
                # properties may inherit from each other and the inner properties do not necessarily have to appear in the pattern hierarchy, we must check and add them too
                loadClassParents(value, properties)

def calculateMatchsets():
    """ Loops over the pattern classes and if a leaf is found, its value is propagated to all its parents matchsets.

    Because parents of patterns can be either patterns themselves, or properties, this takes care of matchsets for both properties and patterns at the same time.
    """
    D("Calculating matchsets")
    for p in patterns.values():
        if p.isLeaf():
            p.addToMatchset(p.name)

def extractMatchMethods():
    """ Extracts the match methods in the pass. """
    D("extracting match methods")
    for h in passes.values():
        h.extractMatchMethods()

def calculateLLVMInstructions():
    """ Calculates used LLVM instruction names by scanning the match methods of known passes.

    Any argument to a match method that is not pattern, property or predicate is considered to be an LLVM instruction.
    """
    D("extracting llvm instructions")
    for h in passes.values():
        for m in h.matchMethods:
            for i in range(len(m.args)):
                argName = m.args[i]
                if (argName not in classes):
                    # check if the name is in proper format with llvm namespace attached, attach if not
                    if (not argName.startswith("llvm::")):
                        argName = "llvm::" + argName
                        m.args[i] = argName
                    D("    {0}".format(argName))
                    instructions.add(argName)


def checkMatchMethodSignatures():
    """ Checks that all match methods have proper signatures.

    This means that the match method must contain at least one instruction, pattern or property argument and may contain at most one predicate as last argument.
    """
    D("checking match methods signatures")
    for h in passes.values():
        for m in h.matchMethods:
            m.checkMatchMethod()

def checkPredicates():
    """ Checks that any predicates used in the matchers contain proper methods and signatures.

    A predicate must contain a method named match which returns bool and has a pass as its first argument, followed by the same arguments as the match method without the predicate itself.
    """
    D("checking predicate match methods signatures")
    for h in passes.values():
        for m in h.matchMethods:
            if (m.predicate):
                predicates[m.predicate].checkPredicateAgainstMatchMethod(m)


def buildDispatchTables():
    """ For each pass, builds a dispatch table to guide the dispatching. Errors upon ambiguous dispatch table.

    The dispatch table for each pass is filled. When ambiguities appear, they are reported as errors.
    """
    D("generating dispatch tables")
    for h in passes.values():
        h.createDispatchTable()
    pass

def emit():
    D("emitting code for dispatch methods")
    for h in passes.values():
        h.emit()

def emitPatternKinds():
    D("emitting pattern kinds")
    filename = cppRoot + "/ir/pattern_kinds.inc"
    pks = set([ i.split("::")[-1] for i in patterns ])
    # check if the new emit differs from the old one, if not don't emit
    if (os.path.isfile(filename)):
        old = set()
        with open(filename,"r") as f:
            f.readline() # the comment
            for line in f:
                # get rid of whitespace and trailing comma
                line = line.strip()[:-1]
                if (not line):
                    continue
                old.add(line)
        if (not force and pks == old):
            D("  skipping")
            return
    # emit
    with open(filename, "w") as f:
        print("/* This file has been automatically generated. Do not hand-edit. */", file = f)
        for pk in pks:
            print("{0},".format(pk), file = f)

# ---------------------------------------------------------------------------------------------------------------------

def main():
    """ Does the codegen. """
    global doxygenRoot
    global patterns
    global properties
    global instructions
    global predicates
    global passes
    global destDir
    global cppRoot
    global clangFormat
    global force
    global verbose


    # parse arguments, first three are given
    doxygenRoot = str(sys.argv[1])
    destDir = str(sys.argv[2])
    clangFormat = str(sys.argv[3])

    # parse additional args
    for i in sys.argv[4:]:
        if (i == "force"):
            force = True
        elif (i == "verbose"):
            verbose = True
        else:
            error("","","Unknown argument {0}".format(i))

    if (not os.path.isdir(doxygenRoot)):
        error("","","Doxygen xml output directory {0} does not exist.".format(doxygenRoot))


    cppRoot = os.path.abspath(str(sys.argv[0]))
    cppRoot = cppRoot[:cppRoot.find("/tools/codegen_passes.py")]
    cppRoot = os.path.join(cppRoot, "rjit", "src")
    D("cppRoot set to {0}".format(cppRoot))

    # load all patterns
    patterns = loadClassHierarchy(PATTERN_BASE)
    # determine property classes by looking at the patterns
    loadProperties()
    # calculate matchsets for patterns & properties
    calculateMatchsets()
    # load all predicates
    predicates = loadClassHierarchy(PREDICATE_BASE)
    # load all passes
    passes = loadClassHierarchy(PASS_BASE)
    # extracts match methods from loaded passes
    extractMatchMethods()
    # when we have all match methods, we will use their arguments to check for llvm instruction types used (because we know arguments can be pattern, properties, predicates, or llvm insns)
    calculateLLVMInstructions()
    # now when we know all classes and types, check for coherence
    checkMatchMethodSignatures()
    # check predicate match methods correspond to the pass match methods where used
    checkPredicates()
    # for each pass, we need to build a match table and error if the match table is ambiguous
    buildDispatchTables()
    # emit the generated code
    emit()
    emitPatternKinds()

    D("DONE")

if __name__ == "__main__":
    main()
