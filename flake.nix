{
  description = "A Nix-flake-based R development environment";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forEachSupportedSystem = f: nixpkgs.lib.genAttrs supportedSystems (system: f {
        pkgs = import nixpkgs { inherit system; overlays = [ self.overlays.default self.overlays.quarto ]; };
      });
    in
    {
      overlays.default = final: prev: {
        rEnv = prev.radianWrapper.override {
          wrapR = true;
          recommendedPackages = prev.radianWrapper.recommendedPackages ++ (with final.rPackages; [ knitr tidyverse sf mapview jsonlite geosphere furrr styler languageserver scales ]);
        };
      };
      overlays.quarto = final: prev: {
        quarto = prev.quarto.override {
          rWrapper = final.rEnv;
        };
      };
      packages =  forEachSupportedSystem ({ pkgs }: {
        default = pkgs.writeShellScriptBin "preview" ''
          ${pkgs.quarto}/bin/quarto check
          ${pkgs.quarto}/bin/quarto preview ./README.qmd --port 42069
          ''; 
      });
      apps = nixpkgs.lib.genAttrs supportedSystems (system: {
        default = {
          type = "app";
          program = "${self.packages.${system}.default}/bin/preview";
        };
      });
      devShells = forEachSupportedSystem ({ pkgs }: {
        default = pkgs.mkShell {
          packages = with pkgs;
            [
              rEnv
              pandoc
              quarto
              glibcLocales
            ];
            shellHook = "
              nu
            ";
          LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
          LANG="es_MX.UTF-8";
          LC_MONETARY="es_MX.UTF-8";
          LC_PAPER="es_MX.UTF-8";
          LC_NAME="es_MX.UTF-8";
          LC_ADDRESS="es_MX.UTF-8";
          LC_TELEPHONE="es_MX.UTF-8";
          LC_MEASUREMENT="es_MX.UTF-8";
          LC_IDENTIFICATION="es_MX.UTF-8";
        };
      });
    };
}
