This is an isolated test-case for `haskell-opencv` issue [#123](https://github.com/LumiGuide/haskell-opencv/issues/123).

The setup is as follows:
* We have a C++ library called `libfoo` which exports a single function `foo` which just throws an exception.
* We have a Haskell library called `hsfoo` which also exports a function named `foo`.
  This function calls the C++ `foo` function using the `inline-c` library but
  wraps it in a `try...catch` block to intercept the C++ exception to turn it into a Haskell exception.
* The `hsfoo` package also provides an executable that just calls the `foo` function.

If we run this executable on Linux we get the expected:

```
> $(nix-build --no-link -A haskellPackages.hsfoo)/bin/test
test: Whoops!
CallStack (from HasCallStack):
  error, called at src/Foo.hs:31:7 in hsfoo-0.0.0.0-HELWcHi5oZ9JRWsbSnIGP5:Foo
```

However, when we run it on OS X the process terminates via signal 6 (SIGABRT):

```
> $(nix-build --no-link -A haskellPackages.hsfoo)/bin/test
...
libc++abi.dylib: terminating with uncaught exception of type std::exception: std::exception
Abort trap: 6
> echo $?
134
```

(Note that exit code 134 [means](https://stackoverflow.com/a/23098735/442793) the program received the SIGABRT signal).

To rule out bugs in `inline-c` or `inlince-c-cpp` I also added a `Foo.Manual` module in `hslib` which exports the `foo_manual` function. Instead of inlining some C++ code this module explicitly provides a FFI call to a manually created `foo_manual` C++ function in `foo-wrapper.cpp` which does the `try...catch` wrapping. Additionally I added a `test-manual` executable which calls `foo_manual`. Unfortunately this executables also aborts.
