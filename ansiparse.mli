open Angstrom

type color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White

type style = Reset | Bold | Faint | Italic | Underline | Blink | Inverse | Hidden
           | Strike | Fore of color | Back of color | Unknown of int

(* Grammar:
   Item --> Escape | Text
   Escape --> csi Styles? cst
   Styles --> Style ( ';' Style )*
   Style --> dig+
   Text --> char*
*)

type item = Esc of style list | Text of string

val item : item t

type run 
