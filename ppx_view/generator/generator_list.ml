type 'a t = {
    add           : 'a -> unit;
    add_from_file : string -> unit;
    write         : string -> unit;
  }

let make reader_func writer_func =
  let items = ref [] in
  let add item =
    items := item :: !items
  in
  let add_list item_list =
    items := (List.rev item_list) @ !items
  in
  let add_from_file path =
    let full_path = Filename.concat !Generator_args.data_path path in
    add_list (reader_func full_path)
  in
  let write path =
    let full_path = Filename.concat !Generator_args.dest_path path in
    let chan = open_out full_path in
    let fmt = Format.formatter_of_out_channel chan in
    writer_func fmt (List.rev !items);
    Format.pp_print_newline fmt ();
    Format.pp_print_flush fmt ();
    close_out chan
  in
  { add; add_from_file; write; }

let make_signature () =
  make Generator_source.parse_signature Generator_source.print_signature

let make_structure () =
  make Generator_source.parse_structure Generator_source.print_structure
