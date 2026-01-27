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
    pkgs.mitscheme
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

  enterShell = "raco pkg install --auto fmt langsever-racket lazy sicp";

  languages = {
    racket.enable = true;
    rust.enable = true;
  };

  scripts.scheme-draw.exec = ''
    if [ -z "$1" ]; then
      racket -l sicp-pict -i
    elif [ "$1" = "--gui" ]; then
      drracket
    elif [ "$1" = "-i" ] && [ -n "$2" ]; then
      racket -i -l sicp-pict -f "$2"
    elif [ "$1" = "--gui" ] && [ -n "$2" ]; then
      drracket "$2"
    else
      racket -l sicp-pict -f "$1"
    fi
  '';

  scripts.scheme.exec = ''
    if [ -z "$1" ]; then
      racket -l sicp -i
    elif [ "$1" = "-i" ] && [ -n "$2" ]; then
      racket -i -l sicp -f "$2"
    else
      racket -l sicp -f "$1"
    fi
  '';
}
