type color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White

module Concrete :
sig
  type style = Bold | Faint | Italic | Underline | Blink | Inverse | Hidden
             | Strike | Fore of color | Back of color | Unknown of int

  (* Grammar:
     Item --> Escape | Text
     Escape --> csi Styles? cst
     Styles --> Style ( ';' Style )*
     Style --> dig+
     Text --> char*
  *)

  type t = Esc of style list | Reset | Text of string

  val parse : in_channel -> t list
end

module Abstract :
sig
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

  val parse : Concrete.t list -> string t

end

module Html :
sig
  (* functions from string Abstract.t -> Html elements *)
  val of_tree : string Abstract.t -> [> Html_types.pre ] Tyxml.Html.elt
end
