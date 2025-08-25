{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-25.05";
  };

  outputs = { self, nixpkgs }: 
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
      c2ffi = pkgs.stdenv.mkDerivation {
        pname = "c2ffi";
        version = "v18.1.0.0";

        src = pkgs.fetchgit {
          url = "https://github.com/rpav/c2ffi";
          rev = "v18.1.0.0";
          hash = "sha256-RoIli6Bbe+mY+p3y4ex1IOdrM2F8IZXJkoRlyUExHA4=";
        };

        nativeBuildInputs = with pkgs; [
          cmake
            clang-tools_18
        ];

        buildInputs = with pkgs; [
          llvmPackages.llvm
            llvmPackages.libclang
            ninja
        ];

        CXXFLAGS = "-fno-rtti";
        shellHook = ''
          alias ..="cd .."
          alias ...="cd ../.."
          '';
      };
    in {
      devShells.${system} = {
        default = pkgs.mkShell {
          shellHook = ''
            PYTHON_BIN=$(dirname `realpath .venv/bin/python`)
            PYTHON_LIB=$(realpath $PYTHON_BIN/../lib)
            export LD_LIBRARY_PATH="$PYTHON_LIB:$LD_LIBRARY_PATH"
            exec elvish
            '';
        };
        header = pkgs.mkShell {
          buildInputs = [
            pkgs.python311
            c2ffi
          ];
          shellHook = ''
            exec elvish
          '';
        };
      };
    };
}
