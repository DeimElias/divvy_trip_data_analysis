{
  description = "A Nix-flake-based R development environment";

  inputs.nixpkgs.url = "https://flakehub.com/f/NixOS/nixpkgs/0.1.*.tar.gz";

  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forEachSupportedSystem = f: nixpkgs.lib.genAttrs supportedSystems (system: f {
        pkgs = import nixpkgs { inherit system; overlays = [ self.overlays.default ]; };
      });
    in
    {
      overlays.default = final: prev: rec {
        rEnv = final.rWrapper.override {
          packages = with final.rPackages; [ knitr tidyverse sf mapview jsonlite geosphere furrr styler quarto];
        };
      };

      devShells = forEachSupportedSystem ({ pkgs }: {
        default = pkgs.mkShell {
          packages = with pkgs;
            [
              rEnv
              pandoc
              glibcLocales
              nix
              quarto
            ];
            shellHook = "
              nu
            ";
            LOCALE_ARCHIVE = "${pkgs.glibcLocalesUtf8}/lib/locale/locale-archive";
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
