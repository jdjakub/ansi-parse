open Angstrom
open Ansiparse

let string_of_styles stys =
  let c = function
    | Black   -> 0
    | Red     -> 1
    | Green   -> 2
    | Yellow  -> 3
    | Blue    -> 4
    | Magenta -> 5
    | Cyan    -> 6
    | White   -> 7
  in
  let f = function
    (* | Reset     -> 0 *)
    | Concrete.Bold      -> 1
    | Faint     -> 2
    | Italic    -> 3
    | Underline -> 4
    | Blink     -> 5
    | Inverse   -> 7
    | Hidden    -> 8
    | Strike    -> 9
    | Fore col  -> 30 + c col
    | Back col  -> 40 + c col
    | Unknown x -> x
  in
  List.map (fun style -> f style |> string_of_int) stys
  |> String.concat ";"

let string_of_item_terminal = function
  | Concrete.Esc styles -> "\x1b[" ^ string_of_styles styles ^ "m"
  | Text str   -> str
  | Reset -> "\x1b[0m"

let string_of_item_readable = function
  | Concrete.Esc styles -> "[style(" ^ string_of_styles styles ^ ")]"
  | Text str   -> str
  | Reset -> "[reset()]"

let per_item_terminal item = print_endline @@ string_of_item_terminal item
let per_item_readable item = print_string @@ string_of_item_readable item
let per_item = ref per_item_terminal
let end_of_line_terminal () = ()
let end_of_line_readable = print_newline
let end_of_line = ref end_of_line_terminal

let per_line line = parse_string ~consume:Consume.All Concrete.Private.items line

let rec driver () = try
    match per_line (input_line stdin) with
    | Ok items -> List.iter !per_item items; !end_of_line (); driver ()
    | Error err -> print_endline err
  with End_of_file -> ()

let () = Arg.(parse [
  "-readable",
  Arg.Unit (fun () -> per_item := per_item_readable; end_of_line := end_of_line_readable),
  "Use readable, expanded tags instead of raw terminal ANSI escape sequences";
] (fun s -> ()) "test3 [-readable]")

let () = driver ()
