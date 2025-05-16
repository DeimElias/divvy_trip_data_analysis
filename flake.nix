{
  description = "A Nix-flake-based R development environment";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forEachSupportedSystem = f: nixpkgs.lib.genAttrs supportedSystems (system: f {
        pkgs = import nixpkgs { inherit system; overlays = [ self.overlays.default ]; };
      });
    in
    {
      overlays.default = final: prev: {
        rEnv = final.radianWrapper.override {
          wrapR = true;
          recommendedPackages = prev.radianWrapper.recommendedPackages ++ (with final.rPackages; [ knitr tidyverse sf mapview jsonlite geosphere furrr styler languageserver scales]);
        };
      };

      devShells = forEachSupportedSystem ({ pkgs }: {
        default = pkgs.mkShell {
          packages = with pkgs;
            [
              rEnv
              pandoc
              nix
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
          QUARTO_R="${pkgs.rEnv}/bin";
        };
      });
    };
}
