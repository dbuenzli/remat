#!/usr/bin/env ocaml
#directory "pkg";;
#use "topkg.ml";;

let () =
  Pkg.describe "remat" ~builder:`OCamlbuild [
    Pkg.bin ~auto:true "src-remat/remat";
    Pkg.bin ~auto:true "src-rematd/rematd";
    Pkg.doc "README.md";
    Pkg.doc "CHANGES.md"; ]
