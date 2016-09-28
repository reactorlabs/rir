#include "code/dataflow.h"

namespace rir {

CodeEditor::Iterator StackV::top = CodeEditor::Iterator(1);
CodeEditor::Iterator StackV::bottom = CodeEditor::Iterator(-1);
}
