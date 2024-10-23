{
  description = "template";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = { self, nixpkgs }: 
  let
    system = "x86_64-linux";
    pkgs = import nixpkgs {
        inherit system;
    };
    deps = with pkgs; [ 
      stdenv.cc.cc
      lldb
      clang
      clang-tools
      linuxPackages_latest.perf
    ];
  in 
  {
    devShells."${system}".default = pkgs.mkShell {
        nativeBuildInputs = deps;
    };
  };  
}
