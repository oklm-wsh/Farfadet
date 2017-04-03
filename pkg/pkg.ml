#!/usr/bin/env ocaml

#directory "pkg";;
#use       "topfind";;
#require   "topkg";;

open Topkg

let faraday = Conf.with_pkg "faraday"
let opam = Pkg.opam_file ~lint_deps_excluding:None "opam"

let () =
  Pkg.describe ~opams:[opam] "farfadet" @@ fun c ->

  let faraday = Conf.value c faraday in

  Ok [ Pkg.lib "pkg/META"
     ; Pkg.doc "README.md"
     ; Pkg.doc "CHANGES.md"
     ; Pkg.lib ~cond:faraday ~exts:Exts.module_library "lib/farfadet"
     ; Pkg.test "test/test" ]
