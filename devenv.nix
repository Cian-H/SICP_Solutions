{
  pkgs,
  lib,
  config,
  inputs,
  ...
}: {
  packages = [
    pkgs.cairo
    pkgs.fontconfig
    pkgs.git
    pkgs.glib
    pkgs.libedit
    pkgs.libjpeg
    pkgs.pango
    pkgs.racket
    pkgs.schemat
    pkgs.stylua
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

  enterShell = "raco pkg install --auto fmt langsever-racket sicp";

  languages.racket.enable = true;

  scripts.scheme.exec = ''
    if [ -z "$1" ]; then
      racket -l sicp -i
    else
      racket -l sicp -f "$1"
    fi
  '';
}
