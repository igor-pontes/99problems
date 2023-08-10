{
  inputs = { nixpkgs.url = "github:nixos/nixpkgs/nixos-23.05"; };

  outputs = { self, nixpkgs }:
  let
    pkgs = nixpkgs.legacyPackages.x86_64-linux.pkgs;
    system = "x86_64-linux";
  in with pkgs; {
    devShells.x86_64-linux.default = mkShell {
      name = "elixir-env";
	    buildInputs = [
	      elixir
	    ];
    };
  };
}
