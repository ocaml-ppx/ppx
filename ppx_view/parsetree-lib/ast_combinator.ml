open Viewlib
open View
open Parseview

let exp_list m view value =
  let rec aux acc = function%view
    | Pexp_construct ({ txt = Lident "::"; _ },
                      Some (Pexp_tuple [hd; tl])) ->
      m hd >>= fun hd -> aux (hd :: acc) tl
    | Pexp_construct ({ txt = Lident "[]"; _ },
                      None) ->
      ok (List.rev acc)
    | _ ->
      error ()
  in
  aux [] value >>++ view
