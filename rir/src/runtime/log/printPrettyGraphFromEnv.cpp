//
// Created by Jakob Hain on 10/10/23.
//

#include "printPrettyGraphFromEnv.h"
#include "R/r.h"
#include "compiler/parameter.h"
#include "runtime/log/printRirObject.h"
#include <fstream>
#include <iostream>
#include <sstream>
#include <sys/stat.h>
#include <unistd.h>

namespace rir {

// TODO: Properly abstract these instead of writing the same code twice
bool pir::Parameter::PIR_GRAPH_PRINT_RIR_OBJECTS =
    getenv("PIR_GRAPH_PRINT_RIR_OBJECTS") != nullptr &&
    strcmp(getenv("PIR_GRAPH_PRINT_RIR_OBJECTS"), "") != 0 &&
    strcmp(getenv("PIR_GRAPH_PRINT_RIR_OBJECTS"), "0") != 0 &&
    strcmp(getenv("PIR_GRAPH_PRINT_RIR_OBJECTS"), "false") != 0;
const char* pir::Parameter::PIR_GRAPH_PRINT_RIR_OBJECTS_PATH =
    getenv("PIR_GRAPH_PRINT_RIR_OBJECTS") != nullptr &&
            strcmp(getenv("PIR_GRAPH_PRINT_RIR_OBJECTS"), "") != 0 &&
            strcmp(getenv("PIR_GRAPH_PRINT_RIR_OBJECTS"), "0") != 0 &&
            strcmp(getenv("PIR_GRAPH_PRINT_RIR_OBJECTS"), "false") != 0 &&
            strcmp(getenv("PIR_GRAPH_PRINT_RIR_OBJECTS"), "1") != 0 &&
            strcmp(getenv("PIR_GRAPH_PRINT_RIR_OBJECTS"), "true") != 0 ?
        getenv("PIR_GRAPH_PRINT_RIR_OBJECTS") : nullptr;
unsigned pir::Parameter::PIR_GRAPH_PRINT_RIR_OBJECTS_FREQUENCY =
    getenv("PIR_GRAPH_PRINT_RIR_OBJECTS_FREQUENCY") != nullptr
        ? strtol(getenv("PIR_GRAPH_PRINT_RIR_OBJECTS_FREQUENCY"), nullptr, 10)
        : 10;

void initializePrintPrettyGraphFromEnv() {
    if (pir::Parameter::PIR_GRAPH_PRINT_RIR_OBJECTS_PATH) {
        // Create folder (not recursively) if it doesn't exist
        auto code = mkdir(pir::Parameter::PIR_GRAPH_PRINT_RIR_OBJECTS_PATH, 0777);
        if (code != 0 && errno != EEXIST) {
            std::cerr << "Could not create folder for PIR_GRAPH_PRINT_RIR_OBJECTS: "
                      << strerror(errno) << std::endl;
            std::abort();
        }
        // Also softlink rirPrettyGraph (HTML dependency) in the folder.
        // We do this even if the folder already exists, because the user may
        // have corrupted it.
        auto linkSource = getenv("PIR_PRETTY_GRAPH_DEPENDENCY_LOCATION");
        assert(linkSource && "PIR_PRETTY_GRAPH_DEPENDENCY_LOCATION should be set by the R executable, we need it to softlink rirPrettyGraph for the HTML prints");
        std::stringstream linkTarget;
        linkTarget << pir::Parameter::PIR_GRAPH_PRINT_RIR_OBJECTS_PATH << "/rirPrettyGraph";
        code = symlink(linkSource, linkTarget.str().c_str());
        if (code != 0 && errno != EEXIST) {
            std::cerr << "Could not symlink associated common styles/scripts for PIR_GRAPH_PRINT_RIR_OBJECTS: "
                      << strerror(errno) << std::endl;
            std::abort();
        }
    }
}

static void printPrettyGraph(SEXP sexp, const std::string& associated) {
    if (pir::Parameter::PIR_GRAPH_PRINT_RIR_OBJECTS_PATH) {
        // Create new file which is denoted by the current date and hash
        std::stringstream filePath;
        filePath << pir::Parameter::PIR_GRAPH_PRINT_RIR_OBJECTS_PATH << "/"
                 << time(nullptr) << "-" << associated << ".html";
        std::ofstream file(filePath.str());
        if (!file.is_open()) {
            std::cerr << "Could not open file for PIR_GRAPH_PRINT_RIR_OBJECTS: "
                      << strerror(errno) << std::endl;
            std::abort();
        }
        // Print HTML pretty graph to file
        printRirObject(sexp, file, RirObjectPrintStyle::PrettyGraph);
        // File closes automatically (RAII)
    } else {
        // Just print HTML pretty graph to stdout
        printRirObject(sexp, std::cout, RirObjectPrintStyle::PrettyGraph);
    }
}

void printPrettyGraphIfNecessary(SEXP sexp, const std::string& associated) {
    static unsigned graphPrintCounter = 0;
    if (pir::Parameter::PIR_GRAPH_PRINT_RIR_OBJECTS) {
        graphPrintCounter++;
        if (graphPrintCounter == pir::Parameter::PIR_GRAPH_PRINT_RIR_OBJECTS_FREQUENCY) {
            printPrettyGraph(sexp, associated);
            graphPrintCounter = 0;
        }
    }
}

void printPrettyGraphOfInternedIfNecessary(SEXP sexp, const UUID& hash) {
    auto associated = hash.str();
    printPrettyGraphIfNecessary(sexp, associated);
}

void printPrettyGraphOfCompiledIfNecessary(SEXP sexp, const std::string& name) {
    printPrettyGraphIfNecessary(sexp, name);
}

} // namespace rir