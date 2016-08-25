open Ansiparse

let concrep = Concrete.parse stdin

let absrep = Abstract.parse concrep

let html = Html.of_tree absrep

let () = Format.printf "%a@." (Tyxml.Html.pp_elt ()) html
