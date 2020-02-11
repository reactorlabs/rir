# LLVM Native Backend

Code in this folder adheres to some extra standards. The reason is because R headers define macros which break LLVM headers, so the LLVM headers must be imported first - the idea is that these standard will help prevent import headaches. The standards are:

- Outside code should only import `lower_llvm.h`, not anything in `internal`
- `lower_llvm.h` doesn't include any LLVM headers
- Inside `internal`
  - `llvm_imports.h` imports all LLVM headers used by the other files.
  - The other header files first import `llvm_imports.h`
  - The other cpp files can just import their respective header first, per regular C++ standards (except `lower_llvm.cpp`), since it will import `llvm_imports.h` first

  If you're ever developing and get weird syntax errors in the LLVM headers, it's probably because the R headers are being included first.