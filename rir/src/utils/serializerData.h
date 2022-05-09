#pragma once

#include "R/r.h"
#include "R/Protect.h"
#include "ir/BC.h"
#include "utils/UMap.h"

#include <fstream>

#define PRINT_EXTENDED_CHILDREN 0

namespace rir {

    class containerDataAbstraction {
        public:
            SEXP container;

            inline void printSpace(int size) {
                assert(size >= 0);
                for (int i = 0; i < size; i++) {
                    std::cout << " ";
                }
            }

            void addFS(rir::FunctionSignature fs, int index) {
                SEXP store;
                PROTECT(store = Rf_allocVector(RAWSXP, sizeof(fs)));
                rir::FunctionSignature * tmp = (rir::FunctionSignature *) DATAPTR(store);
                #if defined(__GNUC__) || defined(__GNUG__)
                    #pragma GCC diagnostic push
                    #pragma GCC diagnostic ignored "-Wclass-memaccess"
                #endif

                memcpy(tmp, &fs, sizeof(fs));

                #if defined(__clang__)
                    #pragma GCC diagnostic pop
                #endif

                SET_VECTOR_ELT(container, index, store);
                UNPROTECT(1);
            }

            rir::FunctionSignature getFS(int index) {
                SEXP dataContainer = VECTOR_ELT(container, index);
                rir::FunctionSignature* res = (rir::FunctionSignature *) DATAPTR(dataContainer);
                return *res;
            }

            void addSizeT(size_t data, int index) {
                SEXP store;
                PROTECT(store = Rf_allocVector(RAWSXP, sizeof(size_t)));
                size_t * tmp = (size_t *) DATAPTR(store);
                *tmp = data;
                SET_VECTOR_ELT(container, index, store);
                UNPROTECT(1);
            }

            size_t getSizeT(int index) {
                SEXP dataContainer = VECTOR_ELT(container, index);
                size_t* res = (size_t *) DATAPTR(dataContainer);
                return *res;
            }

            void addInt(int data, int index) {
                SEXP store;
                PROTECT(store = Rf_allocVector(RAWSXP, sizeof(int)));
                int * tmp = (int *) DATAPTR(store);
                *tmp = data;
                SET_VECTOR_ELT(container, index, store);
                UNPROTECT(1);
            }

            int getInt(int index) {
                SEXP dataContainer = VECTOR_ELT(container, index);
                int* res = (int *) DATAPTR(dataContainer);
                return *res;
            }

            void addUnsigned(unsigned data, int index) {
                SEXP store;
                PROTECT(store = Rf_allocVector(RAWSXP, sizeof(unsigned)));
                unsigned * tmp = (unsigned *) DATAPTR(store);
                *tmp = data;
                SET_VECTOR_ELT(container, index, store);
                UNPROTECT(1);
            }

            unsigned getUnsigned(int index) {
                SEXP dataContainer = VECTOR_ELT(container, index);
                unsigned* res = (unsigned *) DATAPTR(dataContainer);
                return *res;
            }

            void addUnsignedLong(unsigned long data, int index) {
                SEXP store;
                PROTECT(store = Rf_allocVector(RAWSXP, sizeof(unsigned long)));
                unsigned long * tmp = (unsigned long *) DATAPTR(store);
                *tmp = data;
                SET_VECTOR_ELT(container, index, store);
                UNPROTECT(1);
            }

            unsigned long getUnsignedLong(int index) {
                SEXP dataContainer = VECTOR_ELT(container, index);
                unsigned long* res = (unsigned long *) DATAPTR(dataContainer);
                return *res;
            }

            void addString(std::string data, int index) {
                SEXP store;
                PROTECT(store = Rf_mkString(data.c_str()));
                SET_VECTOR_ELT(container, index, store);
                UNPROTECT(1);
            }

            std::string getString(int index) {
                SEXP dataContainer = VECTOR_ELT(container, index);
                return std::string(CHAR(STRING_ELT(dataContainer, 0)));
            }
    };

    class contextData : containerDataAbstraction {
        // {
        // 0 (unsigned long) Context
        // 1 (rir::FunctionSignature) Function Signature
        // 2 (SEXP) Function Names
        // 3 (SEXP) Function Src
        // 4 (SEXP) Function Arglist Order
        // 5 (SEXP) CPool,
        // 6 (SEXP) SPool,
        // 7 (SEXP) Children Data,
        // 8 (SEXP) reqMapForCompilation
        // }
        public:
            SEXP getContainer() {
                return container;
            }
            explicit contextData(SEXP c) {
                container = c;
            }

            // ENTRY 0: Con
            void addContext(unsigned long data) {
                addUnsignedLong(data, 0);
            }

            unsigned long getContext() {
                return getUnsignedLong(0);
            }

            // ENTRY 1: Function Signature
            void addFunctionSignature(rir::FunctionSignature fs) {
                addFS(fs, 1);
            }

            rir::FunctionSignature getFunctionSignature() {
                return getFS(1);
            }

            // ENTRY 2: Function Names
            void addFNames(SEXP data) {
                SET_VECTOR_ELT(container, 2, data);
            }

            SEXP getFNames() {
                return VECTOR_ELT(container, 2);
            }

            // ENTRY 3: Function Src
            void addFSrc(SEXP data) {
                SET_VECTOR_ELT(container, 3, data);
            }

            SEXP getFSrc() {
                return VECTOR_ELT(container, 3);
            }

            // ENTRY 4: Function Arglist Order
            void addFArg(SEXP data) {
                SET_VECTOR_ELT(container, 4, data);
            }

            SEXP getFArg() {
                return VECTOR_ELT(container, 4);
            }

            // ENTRY 5: cPool
            void addCPool(SEXP data) {
                SET_VECTOR_ELT(container, 5, data);
            }

            SEXP getCPool() {
                return VECTOR_ELT(container, 5);
            }

            // ENTRY 6: sPool
            void addSPool(SEXP data) {
                SET_VECTOR_ELT(container, 6, data);
            }

            SEXP getSPool() {
                return VECTOR_ELT(container, 6);
            }

            // ENTRY 7: childrenData
            void addFChildren(SEXP data) {
                SET_VECTOR_ELT(container, 7, data);
            }

            SEXP getFChildren() {
                return VECTOR_ELT(container, 7);
            }

            // ENTRY 8: reqMap
            void addReqMapForCompilation(SEXP data) {
                SET_VECTOR_ELT(container, 8, data);
            }

            SEXP getReqMapAsVector() {
                SEXP rMap = VECTOR_ELT(container, 8);
                return rMap;
            }

            void printToJson(std::ofstream & jsonOutFile, bool prettyJson = false) {

                // Entry 0: Context
                jsonOutFile << "\"context\" : \"" << getContext() << "\",";
                if (prettyJson) jsonOutFile << "\n";

                // Entry 1: Function signature
                rir::FunctionSignature fs = getFunctionSignature();
                jsonOutFile << "\"function_signature\" : [";
                    // fs.envCreation
                    jsonOutFile << (int)fs.envCreation << ",";
                    // fs.optimization
                    jsonOutFile << (int)fs.optimization << ",";
                    // fs.numArguments
                    jsonOutFile << fs.numArguments << ",";
                    // fs.hasDotsFormals
                    jsonOutFile << fs.hasDotsFormals << ",";
                    // fs.hasDefaultArgs
                    jsonOutFile << fs.hasDefaultArgs << ",";
                    // fs.dotsPosition
                    jsonOutFile << fs.dotsPosition;
                jsonOutFile << "],";
                if (prettyJson) jsonOutFile << "\n";

                // Entry 2: Function names
                jsonOutFile << "\"function_names\" : [";
                auto fNames = getFNames();
                for (int i = 0; i < Rf_length(fNames); i++) {
                    auto c = VECTOR_ELT(fNames, i);

                    jsonOutFile << "\"" << CHAR(STRING_ELT(c, 0)) << "\"";
                    if (i+1 != Rf_length(fNames)) jsonOutFile << ",";
                }

                jsonOutFile << "],";
                if (prettyJson) jsonOutFile << "\n";

                // Entry 3: Function Sources
                jsonOutFile << "\"function_sources\" : [";
                auto fSrc = getFSrc();
                for (int i = 0; i < Rf_length(fSrc); i++) {
                    auto c = VECTOR_ELT(fSrc, i);
                    if (c != R_NilValue) {
                        jsonOutFile << "[\"" << CHAR(PRINTNAME(VECTOR_ELT(c, 0))) << "\",\"" << *INTEGER(VECTOR_ELT(c, 1)) << "\"]";
                    } else {
                        jsonOutFile << "null";
                    }
                    if (i+1 != Rf_length(fSrc)) jsonOutFile << ",";
                }

                jsonOutFile << "],";
                if (prettyJson) jsonOutFile << "\n";

                // Entry 4: Function Arglist Order
                jsonOutFile << "\"function_arglist_order\" : [";
                auto fArg = getFArg();
                for (int i = 0; i < Rf_length(fArg); i++) {
                    auto c = VECTOR_ELT(fArg, i);
                    jsonOutFile << "\"" << TYPEOF(c) << "\"";
                    if (i+1 != Rf_length(fArg)) jsonOutFile << ",";
                }

                jsonOutFile << "],";
                if (prettyJson) jsonOutFile << "\n";

                // Entry 5: Constant Pool
                jsonOutFile << "\"cpool\" : [";
                auto cPool = getCPool();
                for (int i = 0; i < Rf_length(cPool); i++) {
                    auto c = VECTOR_ELT(cPool, i);
                    jsonOutFile << "\"" << TYPEOF(c) << "\"";
                    if (i+1 != Rf_length(cPool)) jsonOutFile << ",";
                }
                jsonOutFile << "],";
                if (prettyJson) jsonOutFile << "\n";

                // Entry 6: Source Pool
                jsonOutFile << "\"cpool\" : [";
                auto sPool = getSPool();
                for (int i = 0; i < Rf_length(sPool); i++) {
                    auto c = VECTOR_ELT(sPool, i);
                    jsonOutFile << "\"" << TYPEOF(c) << "\"";
                    if (i+1 != Rf_length(sPool)) jsonOutFile << ",";
                }
                jsonOutFile << "],";
                if (prettyJson) jsonOutFile << "\n";

                // Entry 7: Children Data
                jsonOutFile << "\"children\" : [";
                auto fChildren = getFChildren();
                for (int i = 0; i < Rf_length(fChildren); i++) {
                    auto cVector = VECTOR_ELT(fChildren, i);

                    jsonOutFile << "[";
                    for (int j = 0; j < Rf_length(cVector); j++) {
                        auto d = VECTOR_ELT(cVector, j);

                        jsonOutFile << Rf_asInteger(d);
                        if (j+1 != Rf_length(cVector)) jsonOutFile << ",";

                    }
                    jsonOutFile << "]";
                    if (i+1 != Rf_length(fChildren)) jsonOutFile << ",";
                }
                jsonOutFile << "],";
                if (prettyJson) jsonOutFile << "\n";

                // Entry 8: Requirement map
                jsonOutFile << "\"requirements\" : [";
                auto rData = getReqMapAsVector();
                for (int i = 0; i < Rf_length(rData); i++) {
                    SEXP ele = VECTOR_ELT(rData, i);
                    jsonOutFile << "\"" << CHAR(PRINTNAME(ele)) << "\"";
                    if (i+1 != Rf_length(rData)) jsonOutFile << ",";
                }

                jsonOutFile << "]";
                if (prettyJson) jsonOutFile << "\n";

            }

            void print() {
                print(0);
            }

            void print(int space) {
                printSpace(space);
                std::cout << "context: " << rir::Context(getContext()) << std::endl;
                space += 2;
                printSpace(space);
                std::cout << "ENTRY(0)[context]: " << getContext() << std::endl;
                printSpace(space);
                rir::FunctionSignature fs = getFunctionSignature();
                std::cout << "ENTRY(1)[Function Signature]: " << (int)fs.envCreation << ", " << (int)fs.optimization << ", " <<  fs.numArguments << ", " << fs.hasDotsFormals << ", " << fs.hasDefaultArgs << ", " << fs.dotsPosition << std::endl;
                printSpace(space);
                std::cout << "ENTRY(2)[Function names]: [ ";
                auto fNames = getFNames();
                for (int i = 0; i < Rf_length(fNames); i++) {
                    auto c = VECTOR_ELT(fNames, i);
                    std::cout << CHAR(STRING_ELT(c, 0)) << " ";
                }
                std::cout << "]" << std::endl;

                printSpace(space);
                std::cout << "ENTRY(3)[Function Src]: [ ";
                auto fSrc = getFSrc();
                for (int i = 0; i < Rf_length(fSrc); i++) {
                    auto c = VECTOR_ELT(fSrc, i);
                    if (c != R_NilValue) {
                        std::cout << "(" << CHAR(PRINTNAME(VECTOR_ELT(c, 0))) << "," << *INTEGER(VECTOR_ELT(c, 1)) << ") ";
                    } else {
                        std::cout << "(SRC_NULL) ";
                    }
                }
                std::cout << "]" << std::endl;

                printSpace(space);
                std::cout << "ENTRY(4)[Function Arglist Order]: [ ";
                auto fArg = getFArg();
                for (int i = 0; i < Rf_length(fArg); i++) {
                    auto c = VECTOR_ELT(fArg, i);
                    std::cout << TYPEOF(c) << " ";
                }
                std::cout << "]" << std::endl;

                printSpace(space);
                auto cPool = getCPool();
                std::cout << "ENTRY(5)[CPool]: (" << Rf_length(cPool) << ") [ ";
                for (int i = 0; i < Rf_length(cPool); i++) {
                    auto c = VECTOR_ELT(cPool, i);
                    std::cout << TYPEOF(c) << " ";
                }
                std::cout << "]" << std::endl;

                printSpace(space);
                auto sPool = getSPool();
                std::cout << "ENTRY(6)[SPool]: (" << Rf_length(sPool) << ") [ ";
                for (int i = 0; i < Rf_length(sPool); i++) {
                    auto c = VECTOR_ELT(sPool, i);
                    std::cout << TYPEOF(c) << " ";
                }
                std::cout << "]" << std::endl;

                printSpace(space);
                #if PRINT_EXTENDED_CHILDREN == 1
                std::cout << "ENTRY(7)[children Data]" << std::endl;
                #else
                std::cout << "ENTRY(7)[children Data]: ";
                #endif
                auto fChildren = getFChildren();
                for (int i = 0; i < Rf_length(fChildren); i++) {
                    auto cVector = VECTOR_ELT(fChildren, i);

                    #if PRINT_EXTENDED_CHILDREN == 1
                    printSpace(space);
                    auto handle = std::string(CHAR(STRING_ELT(VECTOR_ELT(fNames, i), 0)));
                    std::cout << handle << " : [ ";
                    #else
                    std::cout << "(" << Rf_length(cVector) << ")" << "[ ";
                    #endif

                    for (int j = 0; j < Rf_length(cVector); j++) {
                        auto d = VECTOR_ELT(cVector, j);

                        #if PRINT_EXTENDED_CHILDREN == 1
                        auto handleC = std::string(CHAR(STRING_ELT(VECTOR_ELT(fNames, Rf_asInteger(d)), 0)));
                        std::cout << handleC << " ";
                        #else
                        std::cout << Rf_asInteger(d) << " ";
                        #endif
                    }
                    std::cout << "] ";
                }

                #if PRINT_EXTENDED_CHILDREN == 0
                std::cout<< std::endl;
                #endif

                auto rData = getReqMapAsVector();
                printSpace(space);
                std::cout << "ENTRY(8)[reqMapForCompilation]: <";
                for (int i = 0; i < Rf_length(rData); i++) {
                    SEXP ele = VECTOR_ELT(rData, i);
                    std::cout << CHAR(PRINTNAME(ele)) << " ";
                }
                std::cout << ">" << std::endl;
            }
    };

    class serializerData : containerDataAbstraction {
        // 0 -> hast
        // 1 -> name
        // 2 -> envData { context: contextData }

        public:
            SEXP getContainer() {
                return container;
            }

            explicit serializerData(SEXP c) {
                container = c;
            }

            serializerData(SEXP c, SEXP hastSym, const std::string & name) {
                container = c;
                SET_VECTOR_ELT(container, 0, hastSym);
                addString(name, 1);
                SEXP envObj;
                PROTECT(envObj = R_NewEnv(R_EmptyEnv,0,0));
                SET_VECTOR_ELT(container, 2, envObj);
                UNPROTECT(1);
            }

            void updateContainer(SEXP c) {
                container = c;
            }

            void addContextData(SEXP cData, int offsetIndex, std::string context) {
                SEXP symSxp = Rf_install(context.c_str());
                SEXP mainMap = VECTOR_ELT(container, 2);
                SEXP offsetSym = Rf_install(std::to_string(offsetIndex).c_str());
                if (Rf_findVarInFrame(mainMap, offsetSym) == R_UnboundValue) {
                    SEXP envObj;
                    PROTECT(envObj = R_NewEnv(R_EmptyEnv,0,0));
                    Rf_defineVar(offsetSym, envObj, mainMap);
                    UNPROTECT(1);
                }
                SEXP offsetMap = Rf_findVarInFrame(mainMap, offsetSym);
                Rf_defineVar(symSxp, cData, offsetMap);
            }

            std::string getNameData() {
                return getString(1);
            }

            SEXP getHastData() {
                return VECTOR_ELT(container, 0);
            }

            SEXP getContextMap() {
                return VECTOR_ELT(container, 2);
            }

            static void iterateOverOffsets(SEXP mainMap, const std::function< void(SEXP, SEXP) >& callback) {

                SEXP offsetBindings = R_lsInternal(mainMap, (Rboolean) false);
                for (int i = 0; i < Rf_length(offsetBindings); i++) {
                    SEXP offsetKeySym = Rf_install(CHAR(STRING_ELT(offsetBindings, i)));
                    SEXP offsetMap = Rf_findVarInFrame(mainMap, offsetKeySym);
                    callback(offsetKeySym, offsetMap);
                }
            }

            static void iterateOverContexts(SEXP offsetMap, const std::function< void(SEXP, SEXP) >& callback) {
                SEXP maskSym = Rf_install("mask");
                SEXP contextBindings = R_lsInternal(offsetMap, (Rboolean) false);
                for (int i = 0; i < Rf_length(contextBindings); i++) {
                    SEXP contextKeySym = Rf_install(CHAR(STRING_ELT(contextBindings, i)));
                    if (contextKeySym == maskSym) {
                        continue;
                    }
                    SEXP contextData = Rf_findVarInFrame(offsetMap, contextKeySym);
                    callback(contextKeySym, contextData);
                }
            }

            void printToJson(std::ofstream & jsonOutFile, bool prettyJson = false) {
                SEXP maskSym = Rf_install("mask");
                jsonOutFile << "\"hast\": \"" << CHAR(PRINTNAME(getHastData())) << "\",";
                if (prettyJson) jsonOutFile << "\n";

                jsonOutFile << "\"name\": \"" << getNameData() << "\",";
                if (prettyJson) jsonOutFile << "\n";

                jsonOutFile << "\"offsetMap\": {";
                if (prettyJson) jsonOutFile << "\n";

                REnvHandler offsetMap(VECTOR_ELT(container, 2));
                int i = 0;
                offsetMap.iterate([&](SEXP offsetSymbol, SEXP cMap) {
                    i++;

                    jsonOutFile << "\"" << CHAR(PRINTNAME(offsetSymbol)) << "\" : {";
                    if (prettyJson) jsonOutFile << "\n";

                    REnvHandler contextMap(cMap);

                    int j = 0;
                    contextMap.iterate([&](SEXP conSym, SEXP cData) {
                        if (conSym == maskSym) {
                            return;
                        }
                        j++;

                        jsonOutFile << "\"" << CHAR(PRINTNAME(conSym)) << "\" : {";
                        if (prettyJson) jsonOutFile << "\n";

                        contextData c(cData);
                        c.printToJson(jsonOutFile, prettyJson);


                        jsonOutFile << "}";
                        if (j != contextMap.size()) {
                            jsonOutFile << ",";
                        }
                        if (prettyJson) jsonOutFile << "\n";
                    });

                    jsonOutFile << "}";
                    if (i != offsetMap.size()) {
                        jsonOutFile << ",";
                    }
                    if (prettyJson) jsonOutFile << "\n";

                });

                jsonOutFile << "}";
                if (prettyJson) jsonOutFile << "\n";


            }

            void print() {
                print(0);
            }


            void print(int space) {
                printSpace(space);
                std::cout << "serializerData: " << std::endl;
                space += 2;
                printSpace(space);
                std::cout << "ENTRY(-3): " << CHAR(PRINTNAME(getHastData())) << std::endl;
                printSpace(space);
                std::cout << "ENTRY(-2): " << getNameData() << std::endl;


                SEXP mainMap = VECTOR_ELT(container, 2);
                space += 2;

                iterateOverOffsets(mainMap, [&] (SEXP offsetSymbol, SEXP offsetEnv) {
                    printSpace(space);
                    std::cout << "offset: " << CHAR(PRINTNAME(offsetSymbol)) << std::endl;
                    iterateOverContexts(offsetEnv, [&] (SEXP contextSym, SEXP cData) {
                        contextData c(cData);
                        c.print(space);
                    });
                });
            }
    };
};;
