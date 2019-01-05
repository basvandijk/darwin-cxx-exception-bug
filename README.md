This is an isolated test-case for `haskell-opencv` issue [#123](https://github.com/LumiGuide/haskell-opencv/issues/123).


Work-around
===========

Adam Sandberg Eriksson just [mentioned on GHC-devs](https://mail.haskell.org/pipermail/ghc-devs/2019-January/016779.html) that there's an existing GHC ticket that covers this problem. See [#11829](https://ghc.haskell.org/trac/ghc/ticket/11829).

The work-around is to add the following to the library component of `hsfoo.cabal`:

```
if os(darwin)
  ld-options: -Wl,-keep_dwarf_unwind
```

I've now added this and it fixes the problem indeed!

The next action is to ask why GHC doesn't pass this option itself when linking on OS X...


Problem description
===================

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


Debugging notes
===============

To rule out bugs in `inline-c` or `inlince-c-cpp` I also added a `Foo.Manual` module in `hslib` which exports the `foo_manual` function. Instead of inlining some C++ code this module explicitly provides a FFI call to a manually created `foo_manual` C++ function in `foo-wrapper.cpp` which does the `try...catch` wrapping. Additionally I added a `test-manual` executable which calls `foo_manual`. Unfortunately this executables also aborts.

I also included a C++ executable in the `foo` directory which calls the `foo()` function wrapped in a `try..catch` block. This executable runs correctly and doesn't abort:

```
> $(nix-build --no-link -A foo)/bin/test
...
[100%] Linking CXX executable test
...
/nix/store/vcgm727r50w7mwy7fxmswhq4ffj2qqf0-clang-wrapper-5.0.2/bin/clang++  -O3 -DNDEBUG -arch x86_64 -Wl,-search_paths_first -Wl,-headerpad_max_install_names  CMakeFiles/test.dir/test.cpp.o  -o test -lfoo
...
Whoops!
```

Note that `clang++` is being used for linking. My suspicion is that GHC is using a different linker. To check if this is true I performed a `cabal new-build -v` and reran the last linking step with `-v`:

```
$ /nix/store/5f5z5bmsmd4v71bazbql2jxp9z2xb8s7-ghc-8.6.3-with-packages/bin/ghc --make -fbuilding-cabal-package -O -static -outputdir /Users/basvandijk/engineering/darwin-cxx-exception-bug/hsfoo/dist-newstyle/build/x86_64-osx/ghc-8.6.3/hsfoo-0.0.0.0/x/test/build/test/test-tmp -odir /Users/basvandijk/engineering/darwin-cxx-exception-bug/hsfoo/dist-newstyle/build/x86_64-osx/ghc-8.6.3/hsfoo-0.0.0.0/x/test/build/test/test-tmp -hidir /Users/basvandijk/engineering/darwin-cxx-exception-bug/hsfoo/dist-newstyle/build/x86_64-osx/ghc-8.6.3/hsfoo-0.0.0.0/x/test/build/test/test-tmp -stubdir /Users/basvandijk/engineering/darwin-cxx-exception-bug/hsfoo/dist-newstyle/build/x86_64-osx/ghc-8.6.3/hsfoo-0.0.0.0/x/test/build/test/test-tmp -i -i/Users/basvandijk/engineering/darwin-cxx-exception-bug/hsfoo/dist-newstyle/build/x86_64-osx/ghc-8.6.3/hsfoo-0.0.0.0/x/test/build/test/test-tmp -itest -i/Users/basvandijk/engineering/darwin-cxx-exception-bug/hsfoo/dist-newstyle/build/x86_64-osx/ghc-8.6.3/hsfoo-0.0.0.0/x/test/build/test/autogen -i/Users/basvandijk/engineering/darwin-cxx-exception-bug/hsfoo/dist-newstyle/build/x86_64-osx/ghc-8.6.3/hsfoo-0.0.0.0/x/test/build/global-autogen -I/Users/basvandijk/engineering/darwin-cxx-exception-bug/hsfoo/dist-newstyle/build/x86_64-osx/ghc-8.6.3/hsfoo-0.0.0.0/x/test/build/test/autogen -I/Users/basvandijk/engineering/darwin-cxx-exception-bug/hsfoo/dist-newstyle/build/x86_64-osx/ghc-8.6.3/hsfoo-0.0.0.0/x/test/build/global-autogen -I/Users/basvandijk/engineering/darwin-cxx-exception-bug/hsfoo/dist-newstyle/build/x86_64-osx/ghc-8.6.3/hsfoo-0.0.0.0/x/test/build/test/test-tmp -optP-include -optP/Users/basvandijk/engineering/darwin-cxx-exception-bug/hsfoo/dist-newstyle/build/x86_64-osx/ghc-8.6.3/hsfoo-0.0.0.0/x/test/build/test/autogen/cabal_macros.h -hide-all-packages -Wmissing-home-modules -no-user-package-db -package-db /Users/basvandijk/.cabal/store/ghc-8.6.3/package.db -package-db /Users/basvandijk/engineering/darwin-cxx-exception-bug/hsfoo/dist-newstyle/packagedb/ghc-8.6.3 -package-db /Users/basvandijk/engineering/darwin-cxx-exception-bug/hsfoo/dist-newstyle/build/x86_64-osx/ghc-8.6.3/hsfoo-0.0.0.0/x/test/package.conf.inplace -package-id base-4.12.0.0 -package-id hsfoo-0.0.0.0-inplace -XHaskell2010 test/test.hs -o /Users/basvandijk/engineering/darwin-cxx-exception-bug/hsfoo/dist-newstyle/build/x86_64-osx/ghc-8.6.3/hsfoo-0.0.0.0/x/test/build/test/test -Wall -hide-all-packages -v
Glasgow Haskell Compiler, Version 8.6.3, stage 2 booted by GHC version 8.2.2
Using binary package database: /nix/store/5f5z5bmsmd4v71bazbql2jxp9z2xb8s7-ghc-8.6.3-with-packages/lib/ghc-8.6.3/package.conf.d/package.cache
Using binary package database: /Users/basvandijk/.cabal/store/ghc-8.6.3/package.db/package.cache
Using binary package database: /Users/basvandijk/engineering/darwin-cxx-exception-bug/hsfoo/dist-newstyle/packagedb/ghc-8.6.3/package.cache
Using binary package database: /Users/basvandijk/engineering/darwin-cxx-exception-bug/hsfoo/dist-newstyle/build/x86_64-osx/ghc-8.6.3/hsfoo-0.0.0.0/x/test/package.conf.inplace/package.cache
package flags [-package-id base-4.12.0.0{unit base-4.12.0.0 True ([])},
               -package-id hsfoo-0.0.0.0-inplace{unit hsfoo-0.0.0.0-inplace True ([])}]
loading package database /nix/store/5f5z5bmsmd4v71bazbql2jxp9z2xb8s7-ghc-8.6.3-with-packages/lib/ghc-8.6.3/package.conf.d
loading package database /Users/basvandijk/.cabal/store/ghc-8.6.3/package.db
loading package database /Users/basvandijk/engineering/darwin-cxx-exception-bug/hsfoo/dist-newstyle/packagedb/ghc-8.6.3
loading package database /Users/basvandijk/engineering/darwin-cxx-exception-bug/hsfoo/dist-newstyle/build/x86_64-osx/ghc-8.6.3/hsfoo-0.0.0.0/x/test/package.conf.inplace
wired-in package ghc-prim mapped to ghc-prim-0.5.3
wired-in package integer-gmp mapped to integer-gmp-1.0.2.0
wired-in package base mapped to base-4.12.0.0
wired-in package rts mapped to rts
wired-in package template-haskell mapped to template-haskell-2.14.0.0
wired-in package ghc mapped to ghc-8.6.3
package flags [-package-id base-4.12.0.0{unit base-4.12.0.0 True ([])},
               -package-id hsfoo-0.0.0.0-inplace{unit hsfoo-0.0.0.0 True ([])}]
loading package database /nix/store/5f5z5bmsmd4v71bazbql2jxp9z2xb8s7-ghc-8.6.3-with-packages/lib/ghc-8.6.3/package.conf.d
loading package database /Users/basvandijk/.cabal/store/ghc-8.6.3/package.db
loading package database /Users/basvandijk/engineering/darwin-cxx-exception-bug/hsfoo/dist-newstyle/packagedb/ghc-8.6.3
loading package database /Users/basvandijk/engineering/darwin-cxx-exception-bug/hsfoo/dist-newstyle/build/x86_64-osx/ghc-8.6.3/hsfoo-0.0.0.0/x/test/package.conf.inplace
wired-in package ghc-prim mapped to ghc-prim-0.5.3
wired-in package integer-gmp mapped to integer-gmp-1.0.2.0
wired-in package base mapped to base-4.12.0.0
wired-in package rts mapped to rts-1.0
wired-in package template-haskell mapped to template-haskell-2.14.0.0
wired-in package ghc mapped to ghc-8.6.3
*** Chasing dependencies:
Chasing modules from: *test/test.hs
!!! Chasing dependencies: finished in 1.86 milliseconds, allocated 2.025 megabytes
Stable obj: [ESfCD :-> Main]
Stable BCO: []
Ready for upsweep
  [NONREC
      ModSummary {
         ms_hs_date = 2019-01-02 14:05:50.729864941 UTC
         ms_mod = Main,
         ms_textual_imps = [(Nothing, Prelude), (Nothing, Foo)]
         ms_srcimps = []
      }]
*** Deleting temp files:
Deleting:
compile: input file test/test.hs
*** Checking old interface for Main (use -ddump-hi-diffs for more details):
[1 of 1] Skipping  Main             ( test/test.hs, /Users/basvandijk/engineering/darwin-cxx-exception-bug/hsfoo/dist-newstyle/build/x86_64-osx/ghc-8.6.3/hsfoo-0.0.0.0/x/test/build/test/test-tmp/Main.o )
Upsweep completely successful.
*** Deleting temp files:
Deleting:
link: linkables are ...
LinkableM (2019-01-03 23:33:55.58053543 UTC) Main
   [DotO /Users/basvandijk/engineering/darwin-cxx-exception-bug/hsfoo/dist-newstyle/build/x86_64-osx/ghc-8.6.3/hsfoo-0.0.0.0/x/test/build/test/test-tmp/Main.o]
Linking /Users/basvandijk/engineering/darwin-cxx-exception-bug/hsfoo/dist-newstyle/build/x86_64-osx/ghc-8.6.3/hsfoo-0.0.0.0/x/test/build/test/test ...
Created temporary directory: /var/folders/8m/tqlhlzrd4lsbb8r3j9zw7wsw0000gn/T/ghc48558_0
*** C Compiler:
/nix/store/vcgm727r50w7mwy7fxmswhq4ffj2qqf0-clang-wrapper-5.0.2/bin/cc -fno-stack-protector -DTABLES_NEXT_TO_CODE -c /var/folders/8m/tqlhlzrd4lsbb8r3j9zw7wsw0000gn/T/ghc48558_0/ghc_1.c -o /var/folders/8m/tqlhlzrd4lsbb8r3j9zw7wsw0000gn/T/ghc48558_0/ghc_2.o -fno-common -U__PIC__ -D__PIC__ -I/nix/store/p2cxpvl8abhfp6dqwpq66ppcpvsyygcy-ghc-8.6.3/lib/ghc-8.6.3/include
*** Linker:
/nix/store/vcgm727r50w7mwy7fxmswhq4ffj2qqf0-clang-wrapper-5.0.2/bin/cc -fno-stack-protector -DTABLES_NEXT_TO_CODE -o /Users/basvandijk/engineering/darwin-cxx-exception-bug/hsfoo/dist-newstyle/build/x86_64-osx/ghc-8.6.3/hsfoo-0.0.0.0/x/test/build/test/test -lm -fno-common -U__PIC__ -D__PIC__ -Wl,-no_compact_unwind /Users/basvandijk/engineering/darwin-cxx-exception-bug/hsfoo/dist-newstyle/build/x86_64-osx/ghc-8.6.3/hsfoo-0.0.0.0/x/test/build/test/test-tmp/Main.o -L/Users/basvandijk/engineering/darwin-cxx-exception-bug/hsfoo/dist-newstyle/build/x86_64-osx/ghc-8.6.3/hsfoo-0.0.0.0/build -L/nix/store/rpxbgapq73vnjm5h4znw7q9l9mb7kna1-libfoo-1.0.0/lib -L/nix/store/fknc220b9gw4kw3nrvkc4mi4a17fzyls-inline-c-cpp-0.3.0.1/lib/ghc-8.6.3/x86_64-osx-ghc-8.6.3/inline-c-cpp-0.3.0.1-FBZfZ2SSmxeEP4rH0wIjJy '-L/nix/store/6pz4mnzdnsyd7j8d65w2c5jphiwib8za-libc++-5.0.2/lib' '-L/nix/store/m6cdh6vm35fp7izsxpjh282cg97swxnq-libc++abi-5.0.2/lib' -L/nix/store/y2fy0p5fg1x4i4nziypg7fjqnw2qs7v8-compiler-rt-5.0.2/lib -L/nix/store/r0pqvcv29gzf9na2a7iik1anbspri8zf-ncurses-6.1-20181027/lib -L/nix/store/ac3ldds9ph2wka4lbwcb1q1glfcxx7sh-gmp-6.1.2/lib -L/nix/store/4fxq4wa5hmpy8r82lsq86r3jj2f1bjzq-libiconv-osx-10.11.6/lib -L/nix/store/x2rx26l2h4h7f4a019rh20sbnb6v1fc5-safe-exceptions-0.1.7.0/lib/ghc-8.6.3/x86_64-osx-ghc-8.6.3/safe-exceptions-0.1.7.0-FZ05XhjB67m4YE68jzkWV6 -L/nix/store/ssz2w6qv01bxxn2c4arm9nlawa3q5jsg-exceptions-0.10.0/lib/ghc-8.6.3/x86_64-osx-ghc-8.6.3/exceptions-0.10.0-DHZ8jRIWmju3Tn1kS7T9Xv -L/nix/store/v1yk2z50hbv1fi088ahdp0fzjvrchsmn-transformers-compat-0.6.2/lib/ghc-8.6.3/x86_64-osx-ghc-8.6.3/transformers-compat-0.6.2-JV1ddSnlm3mL2xw4J4K8Kw -L/nix/store/p2cxpvl8abhfp6dqwpq66ppcpvsyygcy-ghc-8.6.3/lib/ghc-8.6.3/stm-2.5.0.0 -L/nix/store/b7w5y3pkfvg8hfvhng3ps6mfya06d7si-inline-c-0.7.0.1/lib/ghc-8.6.3/x86_64-osx-ghc-8.6.3/inline-c-0.7.0.1-8PCBXs5X7vI9YBSnlMz98z -L/nix/store/mfkq18dnqvhb72hhkjkmsw8r7b9ckchz-vector-0.12.0.2/lib/ghc-8.6.3/x86_64-osx-ghc-8.6.3/vector-0.12.0.2-5G85DK6NPIx1LphnqJy7eO -L/nix/store/p2cxpvl8abhfp6dqwpq66ppcpvsyygcy-ghc-8.6.3/lib/ghc-8.6.3/template-haskell-2.14.0.0 -L/nix/store/p2cxpvl8abhfp6dqwpq66ppcpvsyygcy-ghc-8.6.3/lib/ghc-8.6.3/pretty-1.1.3.6 -L/nix/store/p2cxpvl8abhfp6dqwpq66ppcpvsyygcy-ghc-8.6.3/lib/ghc-8.6.3/ghc-boot-th-8.6.3 -L/nix/store/iyym1a4gnk55ad0ng8fvqnlhqiqxxlgh-parsers-0.12.9/lib/ghc-8.6.3/x86_64-osx-ghc-8.6.3/parsers-0.12.9-KOiKkRqY2RtLMsD43Y8OMK -L/nix/store/wfjimf4sfv0vfmmwrpkxxgcfr55m8w4k-charset-0.3.7.1/lib/ghc-8.6.3/x86_64-osx-ghc-8.6.3/charset-0.3.7.1-9cy5Ez7Z1n2F8LjGWQJvY1 -L/nix/store/l9nx8nlcw01igsc97n9h29q3c0l7rcmw-unordered-containers-0.2.9.0/lib/ghc-8.6.3/x86_64-osx-ghc-8.6.3/unordered-containers-0.2.9.0-L4dSDXPacp4A6ajMikU5GH -L/nix/store/ghyk5ffs3qd3dinhdxfb7ww7f0xv3w0k-semigroups-0.18.5/lib/ghc-8.6.3/x86_64-osx-ghc-8.6.3/semigroups-0.18.5-6T2lH5F6zyQIdwR3JYKMO3 -L/nix/store/afj67794l19lh3r00gwb124imvjq91vr-base-orphans-0.8/lib/ghc-8.6.3/x86_64-osx-ghc-8.6.3/base-orphans-0.8-DkXxj3xyZm03u1pCaf9IQl -L/nix/store/vjrxr81hh20lry5g6cxa68ng7wjafx3v-attoparsec-0.13.2.2/lib/ghc-8.6.3/x86_64-osx-ghc-8.6.3/attoparsec-0.13.2.2-E6LaWejIHPY6hv0KndStJK -L/nix/store/j7a7aq5wi6rdk92iwdrdv8ac3q255hdy-scientific-0.3.6.2/lib/ghc-8.6.3/x86_64-osx-ghc-8.6.3/scientific-0.3.6.2-1REDcXMFLegBiuM4vekkEk -L/nix/store/cv13hzg25rwpnzik1p1mxflykbm2g0w0-primitive-0.6.4.0/lib/ghc-8.6.3/x86_64-osx-ghc-8.6.3/primitive-0.6.4.0-1mvPxVOk6Q6KOkWCZxqESf -L/nix/store/w1prfhbxh61ya0knh3yzkfdy6vdlv4bg-integer-logarithms-1.0.2.2/lib/ghc-8.6.3/x86_64-osx-ghc-8.6.3/integer-logarithms-1.0.2.2-IT1M1moCo0mIobW8LfdrSC -L/nix/store/p2cxpvl8abhfp6dqwpq66ppcpvsyygcy-ghc-8.6.3/lib/ghc-8.6.3/parsec-3.1.13.0 -L/nix/store/p2cxpvl8abhfp6dqwpq66ppcpvsyygcy-ghc-8.6.3/lib/ghc-8.6.3/mtl-2.2.2 -L/nix/store/p2cxpvl8abhfp6dqwpq66ppcpvsyygcy-ghc-8.6.3/lib/ghc-8.6.3/transformers-0.5.5.0 -L/nix/store/q1ww545ya143mvb60vjw0xkpd8yk5dky-hashable-1.2.7.0/lib/ghc-8.6.3/x86_64-osx-ghc-8.6.3/hashable-1.2.7.0-6U3VnAjAfQC5KkN2X3oAD1 -L/nix/store/p2cxpvl8abhfp6dqwpq66ppcpvsyygcy-ghc-8.6.3/lib/ghc-8.6.3/text-1.2.3.1 -L/nix/store/p2cxpvl8abhfp6dqwpq66ppcpvsyygcy-ghc-8.6.3/lib/ghc-8.6.3/binary-0.8.6.0 -L/nix/store/p2cxpvl8abhfp6dqwpq66ppcpvsyygcy-ghc-8.6.3/lib/ghc-8.6.3/containers-0.6.0.1 -L/nix/store/r8z37572cpv0ws6hyr7b0408gwcahii9-ansi-wl-pprint-0.6.8.2/lib/ghc-8.6.3/x86_64-osx-ghc-8.6.3/ansi-wl-pprint-0.6.8.2-9mA7ckPlKRf5aGqviJvBkZ -L/nix/store/gdx945ab4shrxccy2wxm00s4lsirpsmv-ansi-terminal-0.8.2/lib/ghc-8.6.3/x86_64-osx-ghc-8.6.3/ansi-terminal-0.8.2-LZ54B5ErqCiIlNwOVvnj65 -L/nix/store/hfj2i68cr4z8jhcdz3d7i6p98gahlm3f-colour-2.3.4/lib/ghc-8.6.3/x86_64-osx-ghc-8.6.3/colour-2.3.4-BYzTQyKoQ9LK29pbkBo2SY -L/nix/store/p2cxpvl8abhfp6dqwpq66ppcpvsyygcy-ghc-8.6.3/lib/ghc-8.6.3/bytestring-0.10.8.2 -L/nix/store/p2cxpvl8abhfp6dqwpq66ppcpvsyygcy-ghc-8.6.3/lib/ghc-8.6.3/deepseq-1.4.4.0 -L/nix/store/p2cxpvl8abhfp6dqwpq66ppcpvsyygcy-ghc-8.6.3/lib/ghc-8.6.3/array-0.5.3.0 -L/nix/store/p2cxpvl8abhfp6dqwpq66ppcpvsyygcy-ghc-8.6.3/lib/ghc-8.6.3/base-4.12.0.0 -L/nix/store/p2cxpvl8abhfp6dqwpq66ppcpvsyygcy-ghc-8.6.3/lib/ghc-8.6.3/integer-gmp-1.0.2.0 -L/nix/store/p2cxpvl8abhfp6dqwpq66ppcpvsyygcy-ghc-8.6.3/lib/ghc-8.6.3/ghc-prim-0.5.3 -L/nix/store/p2cxpvl8abhfp6dqwpq66ppcpvsyygcy-ghc-8.6.3/lib/ghc-8.6.3/rts /var/folders/8m/tqlhlzrd4lsbb8r3j9zw7wsw0000gn/T/ghc48558_0/ghc_2.o -Wl,-u,_base_GHCziTopHandler_runIO_closure -Wl,-u,_base_GHCziTopHandler_runNonIO_closure -Wl,-u,_ghczmprim_GHCziTuple_Z0T_closure -Wl,-u,_ghczmprim_GHCziTypes_True_closure -Wl,-u,_ghczmprim_GHCziTypes_False_closure -Wl,-u,_base_GHCziPack_unpackCString_closure -Wl,-u,_base_GHCziWeak_runFinalizzerBatch_closure -Wl,-u,_base_GHCziIOziException_stackOverflow_closure -Wl,-u,_base_GHCziIOziException_heapOverflow_closure -Wl,-u,_base_GHCziIOziException_allocationLimitExceeded_closure -Wl,-u,_base_GHCziIOziException_blockedIndefinitelyOnMVar_closure -Wl,-u,_base_GHCziIOziException_blockedIndefinitelyOnSTM_closure -Wl,-u,_base_GHCziIOziException_cannotCompactFunction_closure -Wl,-u,_base_GHCziIOziException_cannotCompactPinned_closure -Wl,-u,_base_GHCziIOziException_cannotCompactMutable_closure -Wl,-u,_base_ControlziExceptionziBase_absentSumFieldError_closure -Wl,-u,_base_ControlziExceptionziBase_nonTermination_closure -Wl,-u,_base_ControlziExceptionziBase_nestedAtomically_closure -Wl,-u,_base_GHCziEventziThread_blockedOnBadFD_closure -Wl,-u,_base_GHCziConcziSync_runSparks_closure -Wl,-u,_base_GHCziConcziIO_ensureIOManagerIsRunning_closure -Wl,-u,_base_GHCziConcziIO_ioManagerCapabilitiesChanged_closure -Wl,-u,_base_GHCziConcziSignal_runHandlersPtr_closure -Wl,-u,_base_GHCziTopHandler_flushStdHandles_closure -Wl,-u,_base_GHCziTopHandler_runMainIO_closure -Wl,-u,_ghczmprim_GHCziTypes_Czh_con_info -Wl,-u,_ghczmprim_GHCziTypes_Izh_con_info -Wl,-u,_ghczmprim_GHCziTypes_Fzh_con_info -Wl,-u,_ghczmprim_GHCziTypes_Dzh_con_info -Wl,-u,_ghczmprim_GHCziTypes_Wzh_con_info -Wl,-u,_base_GHCziPtr_Ptr_con_info -Wl,-u,_base_GHCziPtr_FunPtr_con_info -Wl,-u,_base_GHCziInt_I8zh_con_info -Wl,-u,_base_GHCziInt_I16zh_con_info -Wl,-u,_base_GHCziInt_I32zh_con_info -Wl,-u,_base_GHCziInt_I64zh_con_info -Wl,-u,_base_GHCziWord_W8zh_con_info -Wl,-u,_base_GHCziWord_W16zh_con_info -Wl,-u,_base_GHCziWord_W32zh_con_info -Wl,-u,_base_GHCziWord_W64zh_con_info -Wl,-u,_base_GHCziStable_StablePtr_con_info -Wl,-u,_hs_atomic_add8 -Wl,-u,_hs_atomic_add16 -Wl,-u,_hs_atomic_add32 -Wl,-u,_hs_atomic_add64 -Wl,-u,_hs_atomic_sub8 -Wl,-u,_hs_atomic_sub16 -Wl,-u,_hs_atomic_sub32 -Wl,-u,_hs_atomic_sub64 -Wl,-u,_hs_atomic_and8 -Wl,-u,_hs_atomic_and16 -Wl,-u,_hs_atomic_and32 -Wl,-u,_hs_atomic_and64 -Wl,-u,_hs_atomic_nand8 -Wl,-u,_hs_atomic_nand16 -Wl,-u,_hs_atomic_nand32 -Wl,-u,_hs_atomic_nand64 -Wl,-u,_hs_atomic_or8 -Wl,-u,_hs_atomic_or16 -Wl,-u,_hs_atomic_or32 -Wl,-u,_hs_atomic_or64 -Wl,-u,_hs_atomic_xor8 -Wl,-u,_hs_atomic_xor16 -Wl,-u,_hs_atomic_xor32 -Wl,-u,_hs_atomic_xor64 -Wl,-u,_hs_cmpxchg8 -Wl,-u,_hs_cmpxchg16 -Wl,-u,_hs_cmpxchg32 -Wl,-u,_hs_cmpxchg64 -Wl,-u,_hs_atomicread8 -Wl,-u,_hs_atomicread16 -Wl,-u,_hs_atomicread32 -Wl,-u,_hs_atomicread64 -Wl,-u,_hs_atomicwrite8 -Wl,-u,_hs_atomicwrite16 -Wl,-u,_hs_atomicwrite32 -Wl,-u,_hs_atomicwrite64 -Wl,-search_paths_first -Wl,-dead_strip -lHShsfoo-0.0.0.0-inplace -lHSinline-c-cpp-0.3.0.1-FBZfZ2SSmxeEP4rH0wIjJy -lHSsafe-exceptions-0.1.7.0-FZ05XhjB67m4YE68jzkWV6 -lHSexceptions-0.10.0-DHZ8jRIWmju3Tn1kS7T9Xv -lHStransformers-compat-0.6.2-JV1ddSnlm3mL2xw4J4K8Kw -lHSstm-2.5.0.0 -lHSinline-c-0.7.0.1-8PCBXs5X7vI9YBSnlMz98z -lHSvector-0.12.0.2-5G85DK6NPIx1LphnqJy7eO -lHStemplate-haskell-2.14.0.0 -lHSpretty-1.1.3.6 -lHSghc-boot-th-8.6.3 -lHSparsers-0.12.9-KOiKkRqY2RtLMsD43Y8OMK -lHScharset-0.3.7.1-9cy5Ez7Z1n2F8LjGWQJvY1 -lHSunordered-containers-0.2.9.0-L4dSDXPacp4A6ajMikU5GH -lHSsemigroups-0.18.5-6T2lH5F6zyQIdwR3JYKMO3 -lHSbase-orphans-0.8-DkXxj3xyZm03u1pCaf9IQl -lHSattoparsec-0.13.2.2-E6LaWejIHPY6hv0KndStJK -lHSscientific-0.3.6.2-1REDcXMFLegBiuM4vekkEk -lHSprimitive-0.6.4.0-1mvPxVOk6Q6KOkWCZxqESf -lHSinteger-logarithms-1.0.2.2-IT1M1moCo0mIobW8LfdrSC -lHSparsec-3.1.13.0 -lHSmtl-2.2.2 -lHStransformers-0.5.5.0 -lHShashable-1.2.7.0-6U3VnAjAfQC5KkN2X3oAD1 -lHStext-1.2.3.1 -lHSbinary-0.8.6.0 -lHScontainers-0.6.0.1 -lHSansi-wl-pprint-0.6.8.2-9mA7ckPlKRf5aGqviJvBkZ -lHSansi-terminal-0.8.2-LZ54B5ErqCiIlNwOVvnj65 -lHScolour-2.3.4-BYzTQyKoQ9LK29pbkBo2SY -lHSbytestring-0.10.8.2 -lHSdeepseq-1.4.4.0 -lHSarray-0.5.3.0 -lHSbase-4.12.0.0 -lHSinteger-gmp-1.0.2.0 -lHSghc-prim-0.5.3 -lHSrts -lCffi '-lc++' -lfoo -lgmp -lm -ldl -F/nix/store/xijhnanx3b6c46mdmkpmifvx7smg6f9k-swift-corefoundation/Library/Frameworks -Wl,-dead_strip_dylibs
link: done
*** Deleting temp files:
Deleting: /var/folders/8m/tqlhlzrd4lsbb8r3j9zw7wsw0000gn/T/ghc48558_0/ghc_1.c /var/folders/8m/tqlhlzrd4lsbb8r3j9zw7wsw0000gn/T/ghc48558_0/ghc_3.rsp /var/folders/8m/tqlhlzrd4lsbb8r3j9zw7wsw0000gn/T/ghc48558_0/ghc_4.rsp /var/folders/8m/tqlhlzrd4lsbb8r3j9zw7wsw0000gn/T/ghc48558_0/ghc_2.o
*** Deleting temp dirs:
Deleting: /var/folders/8m/tqlhlzrd4lsbb8r3j9zw7wsw0000gn/T/ghc48558_0
```

As we can see `/nix/store/vcgm727r50w7mwy7fxmswhq4ffj2qqf0-clang-wrapper-5.0.2/bin/cc` is being used for linking. I suspect that if we change this to `/nix/store/vcgm727r50w7mwy7fxmswhq4ffj2qqf0-clang-wrapper-5.0.2/bin/c++` which is a symlink to `clang++` the resulting executable will correctly catch C++ exceptions.

I just tried using `c++` for linking by passing `-pgml c++` to GHC but that didn't help. So my suspicion was wrong...
