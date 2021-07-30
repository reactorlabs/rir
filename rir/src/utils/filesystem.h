#pragma once

#include <string>

// Clear directory contents, create it if missing
int clearOrCreateDirectory(const char* path);

std::string createTmpDirectory();
