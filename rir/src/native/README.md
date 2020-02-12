# LLVM Native Backend

Code in this folder adheres to some extra standards. The reason is because R headers define macros which break LLVM headers, so the LLVM headers must be imported first - the idea is that these standard will help prevent import headaches. The standards are:

- Outside code should only import `lower.h`
- `lower.h` doesn't include any LLVM headers
- `llvm_imports.h` imports all LLVM headers used by the other files.
- All header files except `lower.h` first import `llvm_imports.h`
- All cpp files can just import their respective header first, per regular C++ standards (except `lower.cpp`), since it will import `llvm_imports.h` first
  - Related, `LowerFunction` (in `lower_function.h`) is a big class, so its implementation is split into 3 `.cpp` files within `lower_function`
If you're ever developing and get weird syntax errors in the LLVM headers, it's probably because the R headers are being included first.
