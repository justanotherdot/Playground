#!/usr/bin/env ocaml

type loc = int;;
(*type change = (string, loc);;*)

print_endline "To make `I am a` look like `I am amtrack`...";;


let longest xs ys = if List.length xs > List.length ys then xs else ys

let rec lcs a b = match a,b with
    [], _
  | -, [] -> []
  | x::xs, y::ys ->
    if x = y then
      x :: lcs xs ys
    else
      longest (lcs a ys) (lcs xs b)
