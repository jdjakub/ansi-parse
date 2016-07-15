open Angstrom

type color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White

type style = Reset | Bold | Faint | Italic | Underline | Blink | Inverse | Hidden
           | Strike | Fore of color | Back of color | Unknown of int

type item = Esc of style list | Text of string

let fmt_of_int = function
  | 0 -> Reset | 1 -> Bold | 2 -> Faint | 3 -> Italic | 4 -> Underline
  | 5 -> Blink | 7 -> Inverse | 8 -> Hidden | 9 -> Strike
  | x -> Unknown x

let color_of_int = function
  | 0 -> Black | 1 -> Red     | 2 -> Green | 3 -> Yellow
  | 4 -> Blue  | 5 -> Magenta | 6 -> Cyan  | 7 -> White
  | _ -> assert false

let style_of_int = function
  | x when 30 <= x && x <= 37 -> Fore (color_of_int (x-30))
  | x when 40 <= x && x <= 47 -> Back (color_of_int (x-40))
  | x                         -> fmt_of_int x

(* Grammar:
   Item --> Escape | Text
   Escape --> csi Styles? cst
   Styles --> Style ( ';' Style )*
   Style --> dig+
   Text --> [not start of csi]*
*)

let style = take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string

let styles = sep_by (char ';') style >>| List.map style_of_int

let csi_str = "\x1b["
let csi = string csi_str

let cst = string "m"

let text = peek_char >>= function
  | Some _ -> take_till (fun c -> c = csi_str.[0]) >>| fun str -> Text str
  | None   -> fail "End of input"

let escape = csi *> styles <* cst >>| fun stys -> Esc stys

let item = (escape <|> text) (* >>| fun x ->
  print_endline (match x with | Esc stys -> "Esc" | Text str -> "Text: " ^ str);
  x *)
