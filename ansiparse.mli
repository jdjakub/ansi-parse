type color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White

module Concrete = struct
  type style = Reset | Bold | Faint | Italic | Underline | Blink | Inverse | Hidden
             | Strike | Fore of color | Back of color | Unknown of int

  (* Grammar:
     Item --> Escape | Text
     Escape --> csi Styles? cst
     Styles --> Style ( ';' Style )*
     Style --> dig+
     Text --> char*
  *)

  type t = Esc of style list | Text of string
end

module Abstract = struct
  type weight = Normal | Bold | Faint
  type style = { weight     : weight
               ; italic     : bool
               ; underline  : bool
               ; blink      : bool
               ; reverse    : bool
               ; strike     : bool
               ; foreground : color option
               ; background : color option
               }
  type 'a t = Base of 'a | Styled of style * 'a t list

  val default : style

end

val parse : Concrete.t list -> string Abstract.t
