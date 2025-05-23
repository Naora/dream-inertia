{
  description = "dream-inertia";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-parts.url = "github:hercules-ci/flake-parts"; 
    nix-overlays= { 
      url = "github:nix-ocaml/nix-overlays"; 
      inputs.nixpkgs.follows = "nixpkgs"; 
    };
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, nix-overlays, ... }: 
  flake-parts.lib.mkFlake { inherit inputs; } {
    systems = [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin" ];
    perSystem = { config, system, self', ... }:
    let 
      pkgs = nixpkgs.legacyPackages.${system}.appendOverlays [
        nix-overlays.overlays.default
        (self: super: { ocamlpackages = super.ocaml-ng.ocamlpackages_5_2; })
      ];
      inherit (pkgs) ocamlPackages mkShell;
      inherit (ocamlPackages) buildDunePackage;
      version = "0.0.1+dev";
    in
    {
      packages = {
        default = buildDunePackage {
          inherit version;
          pname = "dream-inertia";
          propagatedBuildInputs = [
            ocamlPackages.dream
            ocamlPackages.fmt
            ocamlPackages.yojson
            ocamlPackages.ppx_yojson_conv
          ];
          src = builtins.filterSource(path: type: !(type == "directory" && baseNameOf path == "examples")) ./.;
        };
      };

      devShells = {
        default = mkShell {
          inputsFrom = [
            self'.packages.default
          ];
          buildInputs = with pkgs; [
            nodejs
            ocamlPackages.ocaml-lsp
            ocamlPackages.ocamlformat
            ocamlPackages.utop
            ocamlPackages.dune-release
            ocamlPackages.odoc
          ];
        };
      };

    }; 
  };
}
