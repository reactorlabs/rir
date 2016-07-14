if (Sys.info()['sysname'] == 'Darwin') {
    dyn.load('librir.dylib')
} else {
    dyn.load('librir.so')
}
sys.source('rir/R/rir.R')
