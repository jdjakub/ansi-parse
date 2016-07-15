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
    | Reset     -> 0
    | Bold      -> 1
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

let string_of_item = function
  | Esc styles -> "\x1b[" ^ string_of_styles styles ^ "m"
  | Text str   -> str

let per_item item = print_endline @@ string_of_item item

let per_line line = parse_only (many item <* end_of_input) (`String line) 
let rec driver () = try
    match per_line (input_line stdin) with
    | Ok items -> List.iter per_item items; driver ()
    | Error err -> print_endline err
  with End_of_file -> ()

let () = driver ()
