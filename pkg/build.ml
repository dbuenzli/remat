#!/usr/bin/env ocaml
#directory "pkg";;
#use "topkg.ml";;

let () =
  Pkg.describe "luigi" ~builder:`OCamlbuild [
    Pkg.bin ~auto:true "src-luigi/luigi";
    Pkg.bin ~auto:true "src-luigid/luigid";
    Pkg.doc "README.md";
    Pkg.doc "CHANGES.md"; ]
