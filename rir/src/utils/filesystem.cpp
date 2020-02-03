#include "filesystem.h"
#include <errno.h>
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

int clearOrCreateDirectory(const char* path) {
    int rv = removeDirectory(path);
    if (rv != 0 && errno != ENOENT)
        return rv;
    return mkdir(path, 0777);
}
