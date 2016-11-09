(** Parse ANSI escape sequences into HTML *)

type color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White

(**
The [Concrete] module is concerned with converting between raw text and a "concrete" list representation of the items in the text.
*)
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


  (** Picture the text as ANSI escape sequences, interspersed with normal text. [parse] converts the raw characters to a [t list]. The 'reset' escape converts to [Reset]; all others convert to [Esc styles], and text to [Text str]. *)
  val parse : in_channel -> t list

  (** Analogous to [parse]. *)
  val parse_str : string -> t list
end

(**
This module translates concrete item lists into an abstract tree structure.
*)
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
  
  (** Styles are applied to ['a]s or further subtrees. *)

  val default : style
  (** The style of the root of the structure; gives plain text *)

  val parse : Concrete.t list -> string t
  (** Converts item lists into trees of styled text. Styles applied to strings will nest until a [Reset] is encountered, after which a new branch from the root is begun. *)

end

(** Backend for style trees -> HTML [<pre>] block *)
module Html :
sig
  (* functions from string Abstract.t -> Html elements *)
  val of_tree : string Abstract.t -> [> Html_types.pre ] Tyxml.Html.elt
end
