final : previous : with final.lib; with final.haskell.lib;
let
  haskellOverrides = self: super:
    let
      addBuildToolsInShell = drv : overrideCabal drv (drv : optionalAttrs inNixShell {
        buildTools = (drv.buildTools or []) ++ (with self; [
          cabal-install
          stack
        ]);
      });
    in {
    hsfoo =
      let drv1 = super.callPackage ./hsfoo/hsfoo.nix {};
          drv2 = addBuildToolsInShell drv1;
      in drv2;
  };
in  {
  libfoo = final.callPackage ./libfoo {};

  foo = final.callPackage ./foo {};

  haskell = previous.haskell // {
    packageOverrides = self: super:
      previous.haskell.packageOverrides self super //
      haskellOverrides self super;
  };
}
