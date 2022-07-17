{
  description = "Flake that configures doom-emacs. Also usable as standalone by exporting DOOM_DIR";
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-22.05";
    nix-doom-emacs.url = "github:nix-community/nix-doom-emacs";
  };

  outputs = { self, nixpkgs, nix-doom-emacs }: {
    nixosModules.default = nixpkgs.lib.mkMerge [
      nix-doom-emacs.hmModule
      ({ ... }: {
        programs.doom-emacs = {
          enable = true;
          doomPrivateDir = ./doom.d;
          extraPackages = epkgs: [ epkgs.evil-terminal-cursor-changer epkgs.openwith ];
        };
      })
    ];
  };
}
