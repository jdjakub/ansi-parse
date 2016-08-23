open Ansiparse

let concrep = Concrete.parse stdin

let absrep = Abstract.parse concrep

let html = Html.of_tree absrep

let () = Tyxml.Html.pp_elt () (Format.formatter_of_out_channel stdout) html
