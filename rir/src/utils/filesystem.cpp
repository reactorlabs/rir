#include "filesystem.h"
#include <errno.h>
#include <ftw.h>
#include <stdio.h>
#include <unistd.h>

// From
// https://stackoverflow.com/questions/5467725/how-to-delete-a-directory-and-its-contents-in-posix-c
static int unlinkCb(const char* fpath, const struct stat* sb, int typeflag,
                    struct FTW* ftwbuf) {
    int rv = remove(fpath);

    if (rv)
        perror(fpath);

    return rv;
}

int removeDirectory(const char* path) {
    return nftw(path, unlinkCb, 64, FTW_DEPTH | FTW_PHYS);
}

int clearOrCreateDirectory(const char* path) {
    int rv = removeDirectory(path);
    if (rv != 0 && errno != ENOENT)
        return rv;
    return mkdir(path, 0777);
}

// From
// https://stackoverflow.com/questions/18792489/how-to-create-a-temporary-directory-in-c
std::string createTmpDirectory(const std::string& pattern) {
    char* p = new char[pattern.size() + 1];
    size_t i = 0;
    for (auto c : pattern)
        p[i++] = c;
    p[i] = 0;
    auto dir = mkdtemp(p);
    if (dir == nullptr)
        perror("mkdtemp failed: ");
    std::string res = dir;
    delete[] p;
    return res;
}
