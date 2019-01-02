This is an isolated test-case for `haskell-opencv` issue [#123](https://github.com/LumiGuide/haskell-opencv/issues/123).

The setup is as follows:
* We have a C++ library called `libfoo` which exports a single function `foo` which when called throws an exception.
* We have a Haskell library called `hsfoo` which also exports a function named `foo`. 
  This function calls the C++ `foo` function using the `inline-c` library but 
  wraps it in a `try...catch` block to intercept the C++ exception and turn it into a Haskell exception.
* The `hsfoo` package also provides an executable that just calls the `foo` function.

If we run this executable on OS X we expect to get a nice Haskell exception. However we get the following:

```
> cd darwin-cxx-exception-bug
> $(nix-build -A haskellPackages.hsfoo)/bin/test
...
libc++abi.dylib: terminating with uncaught exception of type std::exception: std::exception
Abort trap: 6
> echo $?
134
```

Note that the program terminates by receiving signal 6 which is the SIGABRT signal. 
This is also indicated by the [exit code 134](https://stackoverflow.com/a/23098735/442793).
