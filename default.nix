{ reflex-platform ? import ./nix/reflex-platform.nix
, compiler   ? "ghc"
, local-basic-host ? false
} :
let

  pkgs = reflex-platform.nixpkgs.pkgs;
  ghc = reflex-platform.${compiler};

  reflex-basic-host-sources = if local-basic-host then ../reflex-basic-host else (import ./nix/reflex-basic-host.nix);

  reflex-brick-source = ../reflex-brick;

  modifiedHaskellPackages = ghc.override {
    overrides = self: super: {
      reflex-basic-host = self.callPackage reflex-basic-host-sources {};
      reflex-brick = self.callPackage reflex-brick-source {};
      brick = self.callHackage "brick" "0.40" {};
      vty = self.callHackage "vty" "5.23.1" {};
    };
  };

  drv = modifiedHaskellPackages.callPackage ./gitlab-triage.nix {};
in
  drv
