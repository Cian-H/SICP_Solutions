{
  pkgs,
  lib,
  config,
  inputs,
  ...
}: let
  schemeScript = pkgs.writeScriptBin "sicp-scheme" ''
    #!${pkgs.nushell}/bin/nu

    # A unified wrapper for Racket SICP
    def main [
      file?: path       # The scheme file to run (optional)
      --draw            # Use the 'sicp-pict' library instead of 'sicp'
      --gui             # Launch DrRacket GUI
      --interactive (-i) # Enter REPL after running the file
    ] {
      # 1. Handle GUI Mode
      if $gui {
        if ($file != null) {
          exec drracket $file
        } else {
          exec drracket
        }
      }

      # 2. Select Library
      let lib = if $draw { "sicp-pict" } else { "sicp" }

      # 3. Construct Racket Arguments
      # We start with the library definition
      mut args = ["-l", $lib]

      # If no file is provided, or -i is passed, force interactive mode
      if ($file == null) or $interactive {
        $args = ($args | append "-i")
      }

      # If a file is provided, append the file flag
      if ($file != null) {
        $args = ($args | append ["-f", $file])
      }

      # 4. Execute
      exec racket ...$args
    }
  '';

  scheme-langserver = pkgs.stdenv.mkDerivation rec {
    pname = "scheme-langserver";
    version = "2.0.3";

    src = pkgs.fetchurl {
      url = "https://github.com/ufo5260987423/scheme-langserver/releases/download/${version}/scheme-langserver-x86_64-linux-glibc";
      hash = "sha256:8cc5c6c5027dbaa14e51ad7e801e5390bc561c8a04e5653b9d63f35cf6f72c49";
    };
    dontUnpack = true;
    nativeBuildInputs = [pkgs.autoPatchelfHook];
    installPhase = ''
      runHook preInstall
      mkdir -p $out/bin
      cp $src $out/bin/scheme-langserver
      chmod +x $out/bin/scheme-langserver
      runHook postInstall
    '';
  };
in {
  packages = [
    pkgs.cairo
    pkgs.fontconfig
    pkgs.git
    pkgs.glib
    pkgs.libedit
    pkgs.libjpeg
    pkgs.mitscheme
    pkgs.nushell
    pkgs.pango
    pkgs.racket
    pkgs.schemat
    pkgs.stylua
    schemeScript
    scheme-langserver
  ];

  env = {
    LD_LIBRARY_PATH = lib.makeLibraryPath [
      pkgs.cairo
      pkgs.fontconfig.lib
      pkgs.glib
      pkgs.libedit
      pkgs.libjpeg
      pkgs.pango
    ];
  };

  enterShell = "raco pkg install --auto fmt racket-langsever lazy sicp";

  languages = {
    racket.enable = true;
  };
}
