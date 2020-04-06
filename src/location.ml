open! Import

type t = Astlib.Location.t = {
  loc_start : Astlib.Position.t;
  loc_end : Astlib.Position.t;
  loc_ghost : bool;
}

let in_file name =
  let pos : Astlib.Position.t =
    {
      pos_fname=name;
      pos_lnum=1;
      pos_bol=0;
      pos_cnum=(-1);
    }
  in
  {
    loc_start=pos;
    loc_end=pos;
    loc_ghost=true;
  }

let none = in_file "_none_"

let of_lexbuf (lexbuf : Lexing.lexbuf) =
  {
    loc_start=lexbuf.lex_start_p;
    loc_end=lexbuf.lex_curr_p;
    loc_ghost=true;
  }

let print ppf t =
  let start = t.loc_start in
  let end_ = t.loc_end in
  Format.fprintf ppf "File \"%s\", line %d, characters %d-%d:"
    start.pos_fname
    start.pos_lnum
    (start.pos_cnum - start.pos_bol)
    (end_.pos_cnum - end_.pos_bol)

type 'a loc = 'a Astlib.Loc.t

module Error = struct
  type t = Astlib.Location.Error.t

  let createf = Astlib.Location.Error.createf

  let register_of_exn = Astlib.Location.Error.register_of_exn
  let of_exn = Astlib.Location.Error.of_exn
  let to_extension = Error_ext.extension_of_error
end

let report_exception ppf exn =
  match Error.of_exn exn with
  | Some error -> Astlib.Location.Error.report ppf error
  | None -> raise exn

let raise_errorf ?(loc = none) fmt = Astlib.Location.Error.raisef ~loc fmt

exception Error = Astlib.Location.Error.Error

let () =
  Printexc.register_printer (function
    | Error e ->
      let buf = Buffer.create 1024 in
      let ppf = Format.formatter_of_buffer buf in
      Astlib.Location.Error.report ppf e;
      Format.pp_print_flush ppf ();
      Some (Buffer.contents buf)
    | _ -> None)
