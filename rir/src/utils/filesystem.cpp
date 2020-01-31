#include "filesystem.h"
#include <ftw.h>
#include <stdio.h>
#include <unistd.h>

// From
// https://stackoverflow.com/questions/5467725/how-to-delete-a-directory-and-its-contents-in-posix-c
static int unlink_cb(const char* fpath, const struct stat* sb, int typeflag,
                     struct FTW* ftwbuf) {
    int rv = remove(fpath);

    if (rv)
        perror(fpath);

    return rv;
}

int removeDirectory(const char* path) {
    return nftw(path, unlink_cb, 64, FTW_DEPTH | FTW_PHYS);
}