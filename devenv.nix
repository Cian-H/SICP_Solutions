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

  enterShell = "raco pkg install --auto fmt langsever-racket lazy sicp";

  languages = {
    racket.enable = true;
  };
}
