open Ansiparse

let absrep = Abstract.parse stdin in
let html = Html.of_tree absrep in
let () = Tyxml.Html.pp_elt () (formatter_of_out_channel stdout) html
