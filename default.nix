{ reflex-platform ? import ./nix/reflex-platform.nix
, compiler   ? "ghc"
, local-basic-host ? false
} :
let

  pkgs = import <nixpkgs> {};
#  ghc = reflex-platform.${compiler};
  ghc = pkgs.haskell.packages.ghc863;


  reflex-basic-host-sources = if local-basic-host then ../reflex-basic-host else (import ./nix/reflex-basic-host.nix);

  reflex-brick-source = ../reflex-brick;

  modifiedHaskellPackages = ghc.override {
    overrides = self: super: {
      #reflex-basic-host = self.callPackage reflex-basic-host-sources {};
      #reflex-brick = self.callPackage reflex-brick-source {};
      #brick = self.callHackage "brick" "0.40" {};
      #vty = self.callHackage "vty" "5.23.1" {};
      generic-lens = pkgs.haskell.lib.dontCheck super.generic-lens;
#      hspec = pkgs.haskell.lib.dontCheck (self.callHackage "hspec" "2.5.1" {});
      servant = pkgs.haskell.lib.dontCheck super.servant;
      validity = super.validity_0_9_0_0;
      servant-client-core = pkgs.haskell.lib.dontCheck super.servant-client-core;
      servant-server = pkgs.haskell.lib.dontCheck super.servant-server;
      servant-client = pkgs.haskell.lib.dontCheck super.servant-client;
    };
  };

  drv = modifiedHaskellPackages.callPackage ./gitlab-triage.nix {};
in
  drv
