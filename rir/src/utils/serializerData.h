#pragma once

#include "R/r.h"
#include "R/Protect.h"
#include "ir/BC.h"
#include "utils/UMap.h"

namespace rir {

    class containerDataAbstraction {
        public:
            SEXP container;

            void printSpace(int size) {
                for (int i = 0; i < size; i++) {
                    std::cout << " ";
                }
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
        // 0 (unsigned long) context,
        // 1 (int) envCreation,
        // 2 (int) optimization,
        // 3 (unsigned) numArguments,
        // 4 (size_t) dotsPosition,
        // 5 (std::string) mainName,
        // 6 (size_t) cPoolEntriesSize,
        // 7 (size_t) srcPoolEntriesSize,
        // 8 (size_t) promiseSrcPoolEntriesSize,
        // 9 (std::string) childrenData,
        // 10 (std::string) srcData,
        // 11 (std::string) argData,
        // 12 (VECTOR) argOrderingData,
        // 13 (VECTOR) reqMapForCompilation
        // }
        public:
            SEXP getContainer() {
                return container;
            }
            contextData(SEXP c) {
                container = c;
            }

            // ENTRY 0: Con
            void addContext(unsigned long data) {
                addUnsignedLong(data, 0);
            }

            unsigned long getContext() {
                return getUnsignedLong(0);
            }

            // ENTRY 1: EnvCreation
            void addEnvCreation(int data) {
                addInt(data, 1);
            }

            int getEnvCreation() {
                return getInt(1);
            }

            // ENTRY 2: Optimization
            void addOptimization(int data) {
                addInt(data, 2);
            }

            int getOptimization() {
                return getInt(2);
            }

            // ENTRY 3: NumArguments
            void addNumArguments(unsigned data) {
                addUnsigned(data, 3);
            }

            unsigned getNumArguments() {
                return getUnsigned(3);
            }

            // ENTRY 4: DotsPosition
            void addDotsPosition(size_t data) {
                addSizeT(data, 4);
            }

            size_t getDotsPosition() {
                return getSizeT(4);
            }

            // ENTRY 5: MainName
            void addMainName(std::string data) {
                addString(data, 5);
            }

            std::string getMainName() {
                return getString(5);
            }

            // ENTRY 6: cPoolEntriesSize
            void addCPoolEntriesSize(size_t data) {
                addSizeT(data, 6);
            }

            size_t getCPoolEntriesSize() {
                return getSizeT(6);
            }

            // ENTRY 7: srcPoolEntriesSize
            void addSrcPoolEntriesSize(size_t data) {
                addSizeT(data, 7);
            }

            size_t getSrcPoolEntriesSize() {
                return getSizeT(7);
            }

            // ENTRY 8: PromiseSrcPoolEntriesSize
            void addPromiseSrcPoolEntriesSize(size_t data) {
                addSizeT(data, 8);
            }

            size_t getPromiseSrcPoolEntriesSize() {
                return getSizeT(8);
            }

            // ENTRY 9: childrenData
            void addChildrenData(std::string data) {
                addString(data, 9);
            }

            std::string getChildrenData() {
                return getString(9);
            }

            // ENTRY 10: srcData
            void addSrcData(std::string data) {
                addString(data, 10);
            }

            std::string getSrcData() {
                return getString(10);
            }

            // ENTRY 11: argData
            void addArgData(std::string data) {
                addString(data, 11);
            }

            std::string getArgData() {
                return getString(11);
            }

            void addArgOrderingData(SEXP data) {
                SET_VECTOR_ELT(container, 12, data);
            }

            std::vector<std::vector<std::vector<size_t>>> getArgOrderingData() {
                std::vector<std::vector<std::vector<size_t>>> result;
                SEXP aOrderingData = VECTOR_ELT(container, 12);
                for (int i = 0; i < Rf_length(aOrderingData); i++) {
                    SEXP iData = VECTOR_ELT(aOrderingData, i);

                    std::vector<std::vector<size_t>> innerData;
                    for (int j = 0; j < Rf_length(iData); j++) {

                        SEXP jData = VECTOR_ELT(iData, j);

                        std::vector<size_t> innerMostData;

                        for (int k = 0; k < Rf_length(jData); k++) {

                            SEXP dataContainer = VECTOR_ELT(jData, k);
                            size_t* res = (size_t *) DATAPTR(dataContainer);
                            innerMostData.push_back(*res);
                        }
                        innerData.push_back(innerMostData);
                    }
                    result.push_back(innerData);
                }

                return result;
            }

            void addReqMapForCompilation(SEXP data) {
                SET_VECTOR_ELT(container, 13, data);
            }

            std::vector<size_t> getReqMapForCompilation() {
                std::vector<size_t> resData;
                SEXP rMap = VECTOR_ELT(container, 13);
                for (int i = 0; i < Rf_length(rMap); i++) {
                    SEXP dataContainer = VECTOR_ELT(rMap, i);
                    size_t* res = (size_t *) DATAPTR(dataContainer);
                    resData.push_back(*res);
                }
                return resData;
            }

            SEXP getReqMapAsVector() {
                SEXP rMap = VECTOR_ELT(container, 13);
                return rMap;
            }

            void print() {
                std::cout << "context: " << rir::Context(getContext()) << std::endl;
                std::cout << "ENTRY(0)[context]: " << getContext() << std::endl;
                std::cout << "ENTRY(1)[envCreation]: " << getEnvCreation() << std::endl;
                std::cout << "ENTRY(2)[optimization]: " << getOptimization() << std::endl;
                std::cout << "ENTRY(3)[numArguments]: " << getNumArguments() << std::endl;
                std::cout << "ENTRY(4)[dotsPosition]: " << getDotsPosition() << std::endl;
                std::cout << "ENTRY(5)[mainName]: " << getMainName() << std::endl;
                std::cout << "ENTRY(6)[cPoolEntriesSize]: " << getCPoolEntriesSize() << std::endl;
                std::cout << "ENTRY(7)[srcPoolEntriesSize]: " << getSrcPoolEntriesSize() << std::endl;
                std::cout << "ENTRY(8)[promiseSrcPoolEntriesSize]: " << getPromiseSrcPoolEntriesSize() << std::endl;
                std::cout << "ENTRY(9)[childrenData]: " << getChildrenData() << std::endl;
                std::cout << "ENTRY(10)[srcData]: " << getSrcData() << std::endl;
                std::cout << "ENTRY(11)[argData]: " << getArgData() << std::endl;
                auto argOrderingData = getArgOrderingData();
                std::cout << "ENTRY(12)[argOrderingData]: <";
                for (auto & i : argOrderingData) {
                    std::cout << "<";
                    for (auto & j : i) {
                        std::cout << "<";
                        for (auto & ele : j) {
                            std::cout << ele << " ";
                        }
                        std::cout << ">";
                    }
                    std::cout << ">";
                }
                std::cout << ">" << std::endl;

                auto rData = getReqMapForCompilation();
                std::cout << "ENTRY(13)[reqMapForCompilation]: <";
                for (auto & ele : rData) {
                    std::cout << ele << " ";
                }
                std::cout << ">" << std::endl;
            }



            void print(int space) {
                printSpace(space);
                std::cout << "context: " << rir::Context(getContext()) << std::endl;
                space += 2;
                printSpace(space);
                std::cout << "ENTRY(0)[context]: " << getContext() << std::endl;
                printSpace(space);
                std::cout << "ENTRY(1)[envCreation]: " << getEnvCreation() << std::endl;
                printSpace(space);
                std::cout << "ENTRY(2)[optimization]: " << getOptimization() << std::endl;
                printSpace(space);
                std::cout << "ENTRY(3)[numArguments]: " << getNumArguments() << std::endl;
                printSpace(space);
                std::cout << "ENTRY(4)[dotsPosition]: " << getDotsPosition() << std::endl;
                printSpace(space);
                std::cout << "ENTRY(5)[mainName]: " << getMainName() << std::endl;
                printSpace(space);
                std::cout << "ENTRY(6)[cPoolEntriesSize]: " << getCPoolEntriesSize() << std::endl;
                printSpace(space);
                std::cout << "ENTRY(7)[srcPoolEntriesSize]: " << getSrcPoolEntriesSize() << std::endl;
                printSpace(space);
                std::cout << "ENTRY(8)[promiseSrcPoolEntriesSize]: " << getPromiseSrcPoolEntriesSize() << std::endl;
                printSpace(space);
                std::cout << "ENTRY(9)[childrenData]: " << getChildrenData() << std::endl;
                printSpace(space);
                std::cout << "ENTRY(10)[srcData]: " << getSrcData() << std::endl;
                printSpace(space);
                std::cout << "ENTRY(11)[argData]: " << getArgData() << std::endl;
                auto argOrderingData = getArgOrderingData();
                printSpace(space);
                std::cout << "ENTRY(12)[argOrderingData]: <";
                for (auto & i : argOrderingData) {
                    std::cout << "<";
                    for (auto & j : i) {
                        std::cout << "<";
                        for (auto & ele : j) {
                            std::cout << ele << " ";
                        }
                        std::cout << ">";
                    }
                    std::cout << ">";
                }
                std::cout << ">" << std::endl;

                auto rData = getReqMapForCompilation();
                printSpace(space);
                std::cout << "ENTRY(13)[reqMapForCompilation]: <";
                for (auto & ele : rData) {
                    std::cout << ele << " ";
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

            serializerData(SEXP c) {
                container = c;
            }

            serializerData(SEXP c, size_t hast, std::string name) {
                container = c;
                addSizeT(hast, 0);
                addString(name, 1);
                SET_VECTOR_ELT(container, 2, UMap::createMap());
            }

            void updateContainer(SEXP c) {
                container = c;
            }

            void addContextData(SEXP cData, std::string context) {
                SEXP symSxp = Rf_install(context.c_str());
                SEXP map = VECTOR_ELT(container, 2);
                UMap::insert(map, symSxp, cData);
            }

            std::string getNameData() {
                return getString(1);
            }

            size_t getHastData() {
                return getSizeT(0);
            }

            SEXP getContextMap() {
                return VECTOR_ELT(container, 2);
            }

            void print() {
                std::cout << "serializerData: " << std::endl;
                std::cout << "ENTRY(-3): " << getHastData() << std::endl;
                std::cout << "ENTRY(-2): " << getNameData() << std::endl;
                SEXP map = VECTOR_ELT(container, 2);
                std::cout << "ENTRY(-1): " << TYPEOF(map) << std::endl;
                SEXP keys = VECTOR_ELT(map, 0);
                for (int i = 0; i < Rf_length(keys); i++) {
                    SEXP keySym = VECTOR_ELT(keys, i);
                    SEXP ele = UMap::get(map, keySym);
                    contextData c(ele);
                    c.print();
                    // std::cout << i << " " << TYPEOF(ele) << std::endl;
                }

            }

            void print(int space) {
                // std::cout << "serializerData: " << std::endl;
                printSpace(space);
                std::cout << "ENTRY(-3): " << getHastData() << std::endl;
                printSpace(space);
                std::cout << "ENTRY(-2): " << getNameData() << std::endl;
                SEXP map = VECTOR_ELT(container, 2);
                printSpace(space);
                std::cout << "ENTRY(-1): " << TYPEOF(map) << std::endl;
                SEXP keys = VECTOR_ELT(map, 0);
                for (int i = 0; i < Rf_length(keys); i++) {
                    SEXP keySym = VECTOR_ELT(keys, i);
                    SEXP ele = UMap::get(map, keySym);
                    contextData c(ele);
                    c.print(space + 2);
                    // std::cout << i << " " << TYPEOF(ele) << std::endl;
                }

            }
    };
};;
