{
  description = "Flake that configures doom-emacs. Also usable as standalone by exporting DOOM_DIR";
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-22.05";
    nix-doom-emacs.url = "github:nix-community/nix-doom-emacs";
    # File-specific fetching available from Nix 2.9
    # icon = {
    #   url = "file+https://raw.githubusercontent.com/Donnnno/Arcticons/ca3fd9e668a14b478dbe7ed9476e1dbe19742d63/icons/black/editor.svg";
    #   flake = false;
    # };
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
            logview
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
        xdg.desktopEntries = {
          emacs = {
            name = "Emacs";
            genericName = "Text editor";
            exec = "emacs";
            #icon = "${icon}";
          };
        };
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
