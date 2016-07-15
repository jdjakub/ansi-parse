open Angstrom
open Ansiparse

let string_of_style x =
  let c = function
    | Black   -> (0,"Black")
    | Red     -> (1,"Red")
    | Green   -> (2,"Green")
    | Yellow  -> (3,"Yellow")
    | Blue    -> (4,"Blue")
    | Magenta -> (5,"Magenta")
    | Cyan    -> (6,"Cyan")
    | White   -> (7,"White")
  in
  let f = function
    | Reset     -> (0,"Reset")
    | Bold      -> (1,"Bold")
    | Faint     -> (2,"Faint")
    | Italic    -> (3,"Italic")
    | Underline -> (4,"Underline")
    | Blink     -> (5,"Blink")
    | Inverse   -> (7,"Inverse")
    | Hidden    -> (8,"Hidden")
    | Strike    -> (9,"Strike")
    | Fore col  -> let (code,name) = c col in (30+code,name)
    | Back col  -> let (code,name) = c col in (40+code,name)
    | Unknown x -> (x,"???")
  in
  let (code,name) = f x in
  String.concat "" ["\x1b["; string_of_int code; "m"; name; "\x1b[0m"]

let () = read_line ()
         |> (fun line ->
             match parse_only text (`String ("\x1b[m" ^ line)) with
             | Ok pairs -> pairs
                 |> List.map (fun (stys,body) ->
                            "[" ^ (List.map string_of_style stys
                                   |> String.concat ",") ^ "]" ^  body)
                 |> String.concat "\t"
             | Error err -> "Error" ^ err
           )
         |> print_endline

