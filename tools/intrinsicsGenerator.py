import os
import sys




class Intrinsic:
    """ Single intrinsic. 

    Contains the intrinsic name, argument names and argument types and comments to the intrinsic if any. 

    TODO in the future, it also contains the annotations used with the intrinsic so that we can fine-tune its behavior. 
    """

    def __init__(self, annotations, declaration, comment = ""):
        """ Creates the intrinsic from the annotations and declaration lines. """
        self.returnType, declaration = declaration.split(" ", 1)
        self.name, args = declaration.strip()[:-1].split("(")
        # CamelCase the name
        self.className = self.name[0].upper() + self.name[1:]
        if (args):
            args = [ x.strip() for x in args.split(",") ]
            self.argTypes = [ x.split(" ")[0].strip() for x in args]
            self.argNames = [ x.split(" ")[-1].strip() for x in args]
        else:
            self.argTypes = []
            self.argNames = []
        # ignore the annotation for now as it does not tell us anything atm
        # keep the comment, if any, we will add it to the C++ wrapper classfor better clarity
        self.comment = comment
        a = [ x.strip() for x in annotations.split(" ") ]
        i = 1
        while (i < len(a)):
            if (a[i] == "cp"):
                self.markAsConstantPool(a[i + 1])
                i = i + 2
                continue
            else:
                print("Unknown intrinsic annotation modifier {0}".format(a[i]))
                exit()

    def markAsConstantPool(self, argName):
        for i in range(0, len(self.argNames)):
            if (self.argNames[i] == argName):
                self.argTypes[i] = "cp " + self.argTypes[i]
                break;

    def outputArgType(self, index):
        x = self.argTypes[index]
        if (x == "cp SEXP"):
            return "SEXP"
        elif (x == "int"):
            return "int"
        else:
            return "llvm::Value *"

    def argGetterFunction(self, index):
        x = self.argTypes[index]
        if (x == "cp SEXP"):
            return "getValueSEXP"
        elif (x == "int"):
            return "getValueInt"
        else:
            return "getValue"

    def typeToIr(self, type):
        if (type == "SEXP" or type == "cp SEXP"):
            return "t::SEXP"
        elif (type == "int"):
            return "t::Int"
        elif (type == "void"):
            return "t::Void";
        elif (type == "Rboolean"):
            return "t::Int"
        elif (type == "bool"):
            return "t::Bool"
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
        """ Emits the C++ code for the intrinsic that is to be placed in the header - the intrinsic class definition and methods. """
        result = "{0}\nclass {1}: public Intrinsic {{\npublic:\n".format(self.comment, self.className)
        # argument getters
        for i in range(0, len(self.argNames)):
            result += "    " + self.argumentGetterCode(i)
            result += "\n\n"
        # constructor from existing llvm::CallInst instruction
        result += "    {0}(llvm::Instruction * ins):\n        Intrinsic(ins) {{\n    }}\n\n".format(self.className)
        # static constructor
        result += self.staticConstructor()
        # name as returned from static function
        result += "    static char const * intrinsicName() {{\n        return \"{0}\";\n    }}\n\n".format(self.name)
        # type returned from static function as well
        result += "    static llvm::FunctionType * intrinsicType() { \n"
        result += "        return llvm::FunctionType::get({0}, {{ {1} }}, false);\n".format(self.typeToIr(self.returnType), self.irArgTypes())
        result += "    }\n\n"

        result += "};"
        return result

    
    def convertArgumentToValue(self, index):
        x = self.argTypes[index]
        if (x == "cp SEXP"):
            return "Builder::constantPoolSexp({0})".format(self.argNames[index])
        elif (x == "int"):
            return "Builder::integer({0})".format(self.argNames[index])
        else:
            return self.argNames[index];


    def staticConstructor(self):
        """ Returns the c++ code for the static constructor method for the intrinsic call."""
        result = "    static {0} create(Builder & b".format(self.className)
        # signature
        for i in range(0, len(self.argNames)):
            result += ", "
            result += self.outputArgType(i) + " " + self.argNames[i]
        result += ") {\n"
        # any argument that is not llvm::Value must be converted to llvm::Value
        result += "        std::vector<llvm::Value *> args_;\n"
        for i in range(0, len(self.argNames)):
            result +="        args_.push_back({0});\n".format(self.convertArgumentToValue(i))
        result += "        llvm::CallInst * ins = llvm::CallInst::Create(b.intrinsic<{0}>(), args_, \"\", b);\n".format(self.className)
        result += "        b.insertCall(ins);\n" # deal with GC
        result += "        setIRType(ins, ::rjit::ir::Type::{0});\n".format(self.className) # set type in metadata
        result += "        return ins;\n"
        result += "    }\n\n"
        return result

    def argumentGetterCode(self, index):
        return "{0} {1}() {{ return {2}({3}); }}".format(self.outputArgType(index), self.argNames[index], self.argGetterFunction(index), index)




def emit(intrinsics, targetDir):
    """ Extracts all intrinsics that we have into specified header and cpp files. """
    header = open(os.path.join(targetDir, "intrinsics.h"), "w")
    irt = open(os.path.join(targetDir, "irTypes.h"), "w")
    print("#ifndef INTRINSICS_H_\n#define INTRINSICS_H_\n\n#include \"ir.h\"\n#include \"Builder.h\"\n\nnamespace rjit {\nnamespace ir {\n", file = header)
    for i in intrinsics:
        print(i.headerCode(), file = header)
        print("{0}, ".format(i.className), file = irt)

    print("} // namespace ir\n} // namespace rjit\n#endif // INTRINSICS_H_\n", file = header)
    header.close()
    irt.close()



def extractIntrinsics(file, intrinsics):
    """ Extracts the C intrinsic functions from given file, together with their types and any other annotations. 
    
    All annotations are given in C comments blocks starting with /*@ and as of now the following annotations are allowed:

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
                    # we have found the intrinsic marker, the coments and the function follows
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
                    intrinsics.append(Intrinsic(intrinsic, declaration, comment.strip()))
                else:
                    declaration = declaration + line
        return intrinsics

if (len(sys.argv) == 1):
    print("No files to look for intrinsics specified. Exitting. ")
    sys.exit(-1)
# load intrinsics from the files specified.
intrinsics = []
for f in sys.argv[1:]:
    intrinsics = extractIntrinsics(f, intrinsics)
print("Found {0} intrinsic(s) in {1} files...".format(len(intrinsics), len(sys.argv) - 1))
# now create the header and cpp files with the intrinsic definitions
emit(intrinsics, "../rjit/src/ir")
print("intrinsics.cpp, intrinsics.h and irTypes.h generated")

