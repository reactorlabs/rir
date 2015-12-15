#!/usr/bin/env python3

# Note this file will be soon removed as soon as we start adding hierarchy
# to the intrinsic classes. For now, it serves the purpose of quickly
# creating the instruction classes fro the eval.c file.

import os
import sys
import textwrap
import re
from string import Template

FILE_TEMPLATE = Template("""#ifndef INTRINSICS_H_
#define INTRINSICS_H_

#include "Ir.h"
#include "Builder.h"

namespace rjit {
namespace ir {
$content

} // namespace ir
} // namespace rjit
#endif  // INTRINSICS_H_
""")

CLASS_TEMPLATE = Template("""
$comment
class $class_name : public Intrinsic {
  public:

$getters
    $class_name (llvm::Instruction* ins) :
        Intrinsic(ins, InstructionKind::$class_name) { }

$static_ctr

    static char const* intrinsicName() {
        return "$intrinsic_name";
    }

    static llvm::FunctionType* intrinsicType() {
        return llvm::FunctionType::get(
            $return_type,
            {
                $arg_types
            },
            false);
    }

    static bool classof(Instruction const * s) {
        return s->getKind() == InstructionKind::$class_name;
    }
};""")

STATIC_CTR_TEMPLATE = Template("""    static $class_name & create(
            Builder & b$args) {

        std::vector<llvm::Value*> args_;
$args_load
        llvm::CallInst* ins = llvm::CallInst::Create(
            b.intrinsic<$class_name>(),
            args_,
            "",
            b);

        b.insertCall(ins);
        $class_name * result = new $class_name(ins);
        setIR(ins, result);
        //setIRType(ins, InstructionKind::$class_name);
        return *result;
    }""")


class Intrinsic:

    """ Single intrinsic.

    Contains the intrinsic name, argument names and argument types and comments
    to the intrinsic if any.

    TODO in the future, it also contains the annotations used with the
    intrinsic so that we can fine-tune its behavior.  """

    def __init__(self, annotations, declaration, comment=""):
        """ Creates the intrinsic from the annotations and declaration lines.
        """
        self.returnType, declaration = declaration.split(" ", 1)
        if self.returnType == "extern":
            self.returnType, declaration = declaration.split(" ", 1)
        self.name, args = declaration.strip()[:-1].split("(")
        # CamelCase the name
        self.className = self.name[0].upper() + self.name[1:]
        if (args):
            args = [x.strip() for x in args.split(",")]
            self.argTypes = [x.split(" ")[0].strip() for x in args]
            self.argNames = [x.split(" ")[-1].strip() for x in args]
        else:
            self.argTypes = []
            self.argNames = []
        # ignore the annotation for now as it does not tell us anything atm
        # keep the comment, if any, we will add it to the C++ wrapper classfor
        # better clarity
        self.comment = comment
        a = [x.strip() for x in annotations.split(" ")]
        i = 1
        state = ""
        while (i < len(a)):
            if (state == ""):
                if (a[i] == "cp"):
                    state = "consts"
                    i += 1
                    continue
                elif(a[i] == "int"):
                    state = "int"
                    i += 1
                    continue
                else:
                    print(
                        "Unknown intrinsic annotation modifier {0}".format(a[i]))
                    exit()
            elif (state == "consts"):
                self.markAsConstants(a[i])
                i += 1
                state = "cindex"
                continue
            elif (state == "cindex"):
                self.markAsConstantIndex(a[i])
                i += 1
                continue
            elif (state == "int"):
                self.markAsConstInt(a[i])
                i += 1
                continue

    def markAsConstants(self, argName):
        for i in range(0, len(self.argNames)):
            if (self.argNames[i] == argName):
                self.argTypes[i] = "cp"
                break

    def markAsConstantIndex(self, argName):
        for i in range(0, len(self.argNames)):
            if (self.argNames[i] == argName):
                self.argTypes[i] = "ci"
                break

    def markAsConstInt(self, argName):
        for i in range(0, len(self.argNames)):
            if (self.argNames[i] == argName):
                self.argTypes[i] = "constint"
                break

    def outputArgType(self, index):
        x = self.argTypes[index]
        if (x == "cp"):
            return "llvm::Value*"
        elif (x in ("ci", "int", "constint")):
            return "int"
        else:
            return "llvm::Value*"

    def argGetterFunction(self, index):
        x = self.argTypes[index]
        if (x == "cp SEXP"):
            return "getValueSEXP"
        elif (x == "constint"):
            return "getValueInt"
        else:
            return "getValue"

    def typeToIr(self, type):
        if (type in ("SEXP", "cp")):
            return "t::SEXP"
        elif (type in ("ci", "int", "Rboolean", "constint")):
            return "t::Int"
        elif (type == "void"):
            return "t::Void"
        elif (type == "bool"):
            return "t::Bool"
        elif (type == "RCNTXT"):
            return "t::cntxt"
        else:
            print("Unknown type {0}".format(type))
            exit()

    def irArgTypes(self):
        """ Returns the IR types for the arguments """
        result = ""
        for t in self.argTypes:
            if (result != ""):
                result += ", "
            result += self.typeToIr(t)
        return result

    def headerCode(self):
        """ Emits the C++ code for the intrinsic that is to be placed in the
        header - the intrinsic class definition and methods. """

        getters = ""
        # argument getters
        for i in range(0, len(self.argNames)):
            getters += "    " + self.argumentGetterCode(i)
            getters += "\n"
        # static constructor
        staticCtr = self.staticConstructor()

        c = ""
        if self.comment:
            c = re.sub(r"(^//( )?)|(/\*)|(\*/)", "", self.comment)
            c = re.sub(r"^( )?\* ", "", c, 0, re.M)
            c = re.sub(r"\n", " ", c)
            c = "\n".join(textwrap.wrap(c))
            c = re.sub(r"^", "// ", c, 0, re.M)
            c = "\n" + c

        return CLASS_TEMPLATE.substitute(
            comment=c,
                class_name=self.className,
                getters=getters,
                static_ctr=staticCtr,
                intrinsic_name=self.name,
                return_type=self.typeToIr(self.returnType),
                arg_types=self.irArgTypes())

    def staticConstructorArgType(self, index):
        x = self.argTypes[index]
        if (x == "cp"):
            return False
        elif (x == "ci"):
            return "SEXP"
        elif (x == "constint"):
            return "int"
        else:
            return "llvm::Value*"

    def convertArgumentToValue(self, index):
        x = self.argTypes[index]
        if (x == "cp"):
            return "b.consts()"
        elif (x == "ci"):
            return "Builder::integer(b.constantPoolIndex({0}))".format(self.argNames[index])
        elif (x == "constint"):
            return "Builder::integer({0})".format(self.argNames[index])
        else:
            return self.argNames[index]

    def staticConstructor(self):
        """ Returns the c++ code for the static constructor method for the
        intrinsic call.

        Takes constants from builder and SEXPs as constants, these are added to
        the constant pool.
        """
        # signature
        sig = ""
        if len(self.argNames) > 0:
            sig = ",\n"
        for i in range(0, len(self.argNames)):
            x = self.staticConstructorArgType(i)
            if (x):
                sig += "            " + x + " " + self.argNames[i] + ",\n"
        sig = sig[0:-2]

        load = ""
        for i in range(0, len(self.argNames)):
            load += "        args_.push_back({0});\n".format(
                self.convertArgumentToValue(i))

        return STATIC_CTR_TEMPLATE.substitute(
            class_name=self.className,
                args=sig,
                args_load=load)

    def argumentGetterCode(self, index):
        t = self.argTypes[index]
        if (t in ("SEXP", "int")):
            return "llvm::Value* {name}() {{ return getValue({index}); }}".format(name=self.argNames[index], index=index)
        elif (t == "cp"):
            return "llvm::Value* constantPool() {{ return getValue({index}); }}".format(index=index)
        elif (t == "constint"):
            return "int {name}() {{ return getValueInt({index}); }}".format(name=self.argNames[index], index=index)
        elif (t == "ci"):
            return """
    int {name}() {{ return getValueInt({index}); }}
    SEXP {name}Value() {{
        llvm::Function * f = ins()->getParent()->getParent();
        JITModule * m = static_cast<JITModule*>(f->getParent());
        return VECTOR_ELT(m->constPool(f), {name}());
    }}
    SEXP {name}(Builder const & b) {{ return b.constantPool({name}()); }}""".format(name = self.argNames[index], index = index)

        return "{0} {1}() {{ return {2}({3}); }}".format(self.outputArgType(index), self.argNames[index], self.argGetterFunction(index), index)


def emit(intrinsics, targetDir):
    """ Extracts all intrinsics that we have into specified header and cpp files. """
    targetName = os.path.join(targetDir, "Intrinsics.h")

    # irt = open(os.path.join(targetDir, "irTypes.h"), "w")

    res = ""
    for i in intrinsics:
        res += i.headerCode()
        # print("{0}, ".format(i.className), file = irt)

    newHeader = FILE_TEMPLATE.substitute(content=res)

    content = ""
    if os.path.isfile(targetName):
        with open(targetName, 'r') as f:
            content = f.read()
    if not content == newHeader:
        print(targetName, "updated")
        with open(targetName, 'w') as f:
            f.write(newHeader)


def extractIntrinsics(file, intrinsics):
    """ Extracts the C intrinsic functions from given file, together with their
    types and any other annotations.

    All annotations are given in C comments blocks starting with /*@ and as of
    now the following annotations are allowed:

    intrinsic - an intrinsic to be analyzed.

    """
    with open(file, "r") as f:
        SEARCH = 0
        PARSE_INTRINSIC = 1
        PARSE_COMMENT_OR_DECLARATION = 4
        PARSE_COMMENT = 2
        PARSE_DECLARATION = 3

        intrinsic = ""
        comment = ""
        declaration = ""

        state = SEARCH

        for line in f:
            line = line.strip()
            if (not line):
                continue
            if (state == SEARCH):
                if (line.find("/*@intrinsic") == 0):
                    # we have found the intrinsic marker, the coments and the
                    # function follows
                    state = PARSE_INTRINSIC
                    intrinsic = ""
                elif (line.find("//@intrinsic") == 0):
                    intrinsic = line[2:].strip()
                    state = PARSE_COMMENT_OR_DECLARATION
                    comment = ""
                    declaration = ""
                    continue
            if (state == PARSE_INTRINSIC):
                intrinsic = intrinsic + line
                if (line[-2:] == "*/"):
                    intrinsic = intrinsic[2:-2].strip()
                    state = PARSE_COMMENT_OR_DECLARATION
                    comment = ""
                    declaration = ""
                    continue
            if (state == PARSE_COMMENT_OR_DECLARATION):
                if (line[:2] == "//"):
                    comment = comment + line + "\n"
                elif (line[:2] == "/*"):
                    state = PARSE_COMMENT
                else:
                    state = PARSE_DECLARATION
            if (state == PARSE_COMMENT):
                comment = comment + line + "\n"
                if (line[-2:] == "*/"):
                    state = PARSE_COMMENT_OR_DECLARATION
            elif (state == PARSE_DECLARATION):
                if (line.find("{") != 0):
                    line = line[:line.find("{")]
                    state = SEARCH
                    declaration = declaration + line + " "
                    intrinsics.append(
                        Intrinsic(intrinsic, declaration, comment.strip()))
                else:
                    declaration = declaration + line
        return intrinsics

def main():
    if (len(sys.argv) != 3):
        print("usage: " + sys.argv[0] + " inFile.c [inFileN.c +] out/dir")
        sys.exit(-1)

    # load intrinsics from the files specified.
    intrinsics = []
    for f in sys.argv[1:-1]:
        intrinsics = extractIntrinsics(f, intrinsics)

    # now create the header and cpp files with the intrinsic definitions
    emit(intrinsics, sys.argv[-1])

if __name__ == "__main__":
    main()
