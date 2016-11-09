#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "ansiparse" @@ fun c ->
  Ok [ Pkg.mllib "ansiparse.mllib" ]
