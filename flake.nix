{
  description = "Flake that configures doom-emacs. Also usable as standalone by exporting DOOM_DIR";
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-22.05";
    nix-doom-emacs.url = "github:nix-community/nix-doom-emacs";
  };

  outputs = { self, nixpkgs, nix-doom-emacs }: {
    nixosModules.default = nixpkgs.lib.mkMerge [
      nix-doom-emacs.hmModule
      ({ pkgs, ... }: {
        programs.emacs = {
          extraPackages = epkgs: with epkgs; [
            evil-terminal-cursor-changer
            openwith
            org-download
            tree-sitter
            tree-sitter-langs
            nixpkgs-fmt
            # Broken as of Jul 18 2022
            # org-transclusion
          ];
        };
        home.packages = with pkgs; [
          aspell
          aspellDicts.en
          aspellDicts.ru
          aspellDicts.en-computers
          aspellDicts.en-science
          nixpkgs-fmt
        ];
        home.file.".aspell.conf".text = "data-dir ${pkgs.aspell}/lib/aspell";
      })
      ({ ... }: {
        programs.doom-emacs = {
          enable = true;
          doomPrivateDir = ./doom.d;
        };
      })
    ];
  };
}
