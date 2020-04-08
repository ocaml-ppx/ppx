module T = struct
  type 'a map = 'a -> 'a
  type 'a iter = 'a -> unit
  type ('a, 'acc) fold = 'a -> 'acc -> 'acc
  type ('a, 'acc) fold_map = 'a -> 'acc -> ('a * 'acc)
  type ('ctx, 'a) map_with_context = 'ctx -> 'a -> 'a
  type ('a, 'res) lift = 'a -> 'res
end

class map =
  let any x = x in
  object(self)
    method int    : int    T.map = any
    method string : string T.map = any
    method bool   : bool   T.map = any
    method char   : char   T.map = any

    method option : 'a. 'a T.map -> 'a option T.map = fun f x ->
      match x with
      | None -> None
      | Some x -> Some (f x)
    method list : 'a. 'a T.map -> 'a list T.map = List.map
    method array : 'a. 'a T.map -> 'a array T.map = Array.map

    method position : Astlib.Position.t T.map = fun x ->
      let open Astlib.Position in
      let pos_fname = self#string x.pos_fname in
      let pos_lnum = self#int x.pos_lnum in
      let pos_bol = self#int x.pos_bol in
      let pos_cnum = self#int x.pos_cnum in
      { pos_fname; pos_lnum; pos_bol; pos_cnum }

    method location : Astlib.Location.t T.map = fun x ->
      let open Astlib.Location in
      let loc_start = self#position x.loc_start in
      let loc_end = self#position x.loc_end in
      let loc_ghost = self#bool x.loc_ghost in
      { loc_start; loc_end; loc_ghost }

    method loc : 'a. 'a T.map -> 'a Astlib.Loc.t T.map = fun f x ->
      let open Astlib.Loc in
      let txt = f x.txt in
      let loc = self#location x.loc in
      { txt; loc }
  end

class iter =
  let any = ignore in
  object(self)
    method int    : int    T.iter = any
    method string : string T.iter = any
    method bool   : bool   T.iter = any
    method char   : char   T.iter = any

    method option : 'a. 'a T.iter -> 'a option T.iter = fun f x ->
      match x with
      | None -> ()
      | Some x -> f x
    method list : 'a. 'a T.iter -> 'a list T.iter = List.iter
    method array : 'a. 'a T.iter -> 'a array T.iter = Array.iter

    method position : Astlib.Position.t T.iter = fun x ->
      let open Astlib.Position in
      self#string x.pos_fname;
      self#int x.pos_lnum;
      self#int x.pos_bol;
      self#int x.pos_cnum

    method location : Astlib.Location.t T.iter = fun x ->
      let open Astlib.Location in
      self#position x.loc_start;
      self#position x.loc_end;
      self#bool x.loc_ghost

    method loc : 'a. 'a T.iter -> 'a Astlib.Loc.t T.iter = fun f x ->
      let open Astlib.Loc in
      f x.txt;
      self#location x.loc
  end

class ['acc] fold =
  let any _ acc = acc in
  object(self)
    method int    : (int    , 'acc) T.fold = any
    method string : (string , 'acc) T.fold = any
    method bool   : (bool   , 'acc) T.fold = any
    method char   : (char   , 'acc) T.fold = any

    method option : 'a. ('a, 'acc) T.fold -> ('a option, 'acc) T.fold = fun f x acc ->
      match x with
      | None -> acc
      | Some x -> f x acc

    method list : 'a. ('a, 'acc) T.fold -> ('a list, 'acc) T.fold =
      let rec loop f l acc =
        match l with
        | [] -> acc
        | x :: l -> loop f l (f x acc)
      in
      loop

    method array : 'a. ('a, 'acc) T.fold -> ('a array, 'acc) T.fold = fun f a acc ->
      let r = ref acc in
      for i = 0 to Array.length a - 1 do
        r := f (Array.unsafe_get a i) !r
      done;
      !r

    method position : (Astlib.Position.t, 'acc) T.fold = fun x acc ->
      let open Astlib.Position in
      let acc = self#string x.pos_fname acc in
      let acc = self#int x.pos_lnum acc in
      let acc = self#int x.pos_bol acc in
      self#int x.pos_cnum acc

    method location : (Astlib.Location.t, 'acc) T.fold = fun x acc ->
      let open Astlib.Location in
      let acc = self#position x.loc_start acc in
      let acc = self#position x.loc_end acc in
      self#bool x.loc_ghost acc

    method loc : 'a. ('a, 'acc) T.fold -> ('a Astlib.Loc.t, 'acc) T.fold = fun f x acc ->
      let open Astlib.Loc in
      let acc = f x.txt acc in
      self#location x.loc acc
  end

class ['acc] fold_map =
  let any x acc = (x, acc) in
  object(self)
    method int    : (int    , 'acc) T.fold_map = any
    method string : (string , 'acc) T.fold_map = any
    method bool   : (bool   , 'acc) T.fold_map = any
    method char   : (char   , 'acc) T.fold_map = any

    method option : 'a. ('a, 'acc) T.fold_map -> ('a option, 'acc) T.fold_map
      = fun f x acc ->
        match x with
        | None -> (None, acc)
        | Some x -> let x, acc = f x acc in (Some x, acc)

    method list : 'a. ('a, 'acc) T.fold_map -> ('a list, 'acc) T.fold_map =
      let rec loop f l acc =
        match l with
        | [] -> ([], acc)
        | x :: l ->
          let x, acc = f x acc in
          let l, acc = loop f l acc in
          (x :: l, acc)
      in
      loop

    method array : 'a. ('a, 'acc) T.fold_map -> ('a array, 'acc) T.fold_map
      = fun f a acc ->
        let len = Array.length a in
        if len = 0 then
          (a, acc)
        else begin
          let x, acc = f (Array.unsafe_get a 0) acc in
          let a' = Array.make len x in
          let r = ref acc in
          for i = 1 to len - 1 do
            let x, acc = f (Array.unsafe_get a i) !r in
            Array.unsafe_set a' i x;
            r := acc
          done;
          (a', !r)
        end

    method position : (Astlib.Position.t, 'acc) T.fold_map = fun x acc ->
      let open Astlib.Position in
      let (pos_fname, acc) = self#string x.pos_fname acc in
      let (pos_lnum, acc) = self#int x.pos_lnum acc in
      let (pos_bol, acc) = self#int x.pos_bol acc in
      let (pos_cnum, acc) = self#int x.pos_cnum acc in
      ({ pos_fname; pos_lnum; pos_bol; pos_cnum }, acc)

    method location : (Astlib.Location.t, 'acc) T.fold_map = fun x acc ->
      let open Astlib.Location in
      let (loc_start, acc) = self#position x.loc_start acc in
      let (loc_end, acc) = self#position x.loc_end acc in
      let (loc_ghost, acc) = self#bool x.loc_ghost acc in
      ({ loc_start; loc_end; loc_ghost }, acc)

    method loc :
      'a. ('a, 'acc) T.fold_map -> ('a Astlib.Loc.t, 'acc) T.fold_map =
      fun f x acc ->
      let open Astlib.Loc in
      let (txt, acc) = f x.txt acc in
      let (loc, acc) = self#location x.loc acc in
      ({ txt; loc }, acc)
  end

class ['ctx] map_with_context =
  let any _ x = x in
  object(self)
    method int    : ('ctx, int   ) T.map_with_context = any
    method string : ('ctx, string) T.map_with_context = any
    method bool   : ('ctx, bool  ) T.map_with_context = any
    method char   : ('ctx, char  ) T.map_with_context = any

    method option
      : 'a. ('ctx, 'a) T.map_with_context -> ('ctx, 'a option) T.map_with_context
      = fun f ctx x ->
        match x with
        | None -> None
        | Some x -> Some (f ctx x)

    method list
      : 'a. ('ctx, 'a) T.map_with_context -> ('ctx, 'a list) T.map_with_context
      = fun f ctx l -> List.map (f ctx) l

    method array
      : 'a. ('ctx, 'a) T.map_with_context -> ('ctx, 'a array) T.map_with_context
      = fun f ctx a -> Array.map (f ctx) a

    method position : ('ctx, Astlib.Position.t) T.map_with_context = fun ctx x ->
      let open Astlib.Position in
      let pos_fname = self#string ctx x.pos_fname in
      let pos_lnum = self#int ctx x.pos_lnum in
      let pos_bol = self#int ctx x.pos_bol in
      let pos_cnum = self#int ctx x.pos_cnum in
      { pos_fname; pos_lnum; pos_bol; pos_cnum }

    method location : ('ctx, Astlib.Location.t) T.map_with_context = fun ctx x ->
      let open Astlib.Location in
      let loc_start = self#position ctx x.loc_start in
      let loc_end = self#position ctx x.loc_end in
      let loc_ghost = self#bool ctx x.loc_ghost in
      { loc_start; loc_end; loc_ghost }

    method loc
      : 'a. ('ctx, 'a) T.map_with_context ->
        ('ctx, 'a Astlib.Loc.t) T.map_with_context
      = fun f ctx x ->
      let open Astlib.Loc in
      let txt = f ctx x.txt in
      let loc = self#location ctx x.loc in
      { txt; loc }
  end

class virtual ['res] lift =
  object(self)
    method virtual other     : 'a. ('a,        'res) T.lift
    method virtual int       :     (int ,      'res) T.lift
    method virtual string    :     (string,    'res) T.lift
    method virtual bool      :     (bool ,     'res) T.lift
    method virtual char      :     (char ,     'res) T.lift
    method virtual array     : 'a. ('a,        'res) T.lift -> ('a array, 'res) T.lift
    method virtual float     :     (float,     'res) T.lift
    method virtual int32     :     (int32,     'res) T.lift
    method virtual int64     :     (int64,     'res) T.lift
    method virtual nativeint :     (nativeint, 'res) T.lift
    method virtual unit      :     (unit,      'res) T.lift

    method virtual node : (string * int) option -> 'res -> 'res
    method virtual record : (string * int) option -> (string * 'res) list -> 'res
    method virtual constr : (string * int) option -> string -> 'res list -> 'res
    method virtual tuple : 'res list -> 'res

    method option : 'a. ('a, 'res) T.lift -> ('a option, 'res) T.lift = fun f x ->
      match x with
      | None -> self#constr None "None" []
      | Some x -> self#constr None "Some" [f x]

    method list : 'a. ('a, 'res) T.lift -> ('a list, 'res) T.lift = fun f l ->
      match l with
      | [] -> self#constr None "[]" []
      | x :: l -> self#constr None "::" [f x; self#list f l]

    method position : (Astlib.Position.t, 'res) T.lift = fun x ->
      let open Astlib.Position in
      let pos_fname = self#string x.pos_fname in
      let pos_lnum = self#int x.pos_lnum in
      let pos_bol = self#int x.pos_bol in
      let pos_cnum = self#int x.pos_cnum in
      self#record
        None
        [ ("pos_fname", pos_fname)
        ; ("pos_lnum", pos_lnum)
        ; ("pos_bol", pos_bol)
        ; ("pos_cnum", pos_cnum) ]

    method location : (Astlib.Location.t, 'res) T.lift = fun x ->
      let open Astlib.Location in
      let loc_start = self#position x.loc_start in
      let loc_end = self#position x.loc_end in
      let loc_ghost = self#bool x.loc_ghost in
      self#record
        None
        [("loc_start", loc_start); ("loc_end", loc_end); ("loc_ghost", loc_ghost)]

    method loc : 'a. ('a, 'res) T.lift -> ('a Astlib.Loc.t, 'res) T.lift
      = fun f x ->
      let open Astlib.Loc in
      let txt = f x.txt in
      let loc = self#location x.loc in
      self#record None [("txt", txt); ("loc", loc)]
  end

class type ['res] std_lifters =
  object
    method other     : 'a. ('a,    'res) T.lift
    method int       : (int ,      'res) T.lift
    method string    : (string,    'res) T.lift
    method bool      : (bool ,     'res) T.lift
    method char      : (char ,     'res) T.lift
    method array     : 'a. ('a,    'res) T.lift -> ('a array, 'res) T.lift
    method node      : (string * int) option ->             'res       -> 'res
    method record    : (string * int) option -> (string *   'res) list -> 'res
    method constr    : (string * int) option -> string ->   'res  list -> 'res
    method tuple     : 'res list   ->    'res
    method float     : (float,     'res) T.lift
    method int32     : (int32,     'res) T.lift
    method int64     : (int64,     'res) T.lift
    method nativeint : (nativeint, 'res) T.lift
    method unit      : (unit,      'res) T.lift
    method option    : 'a. ('a,    'res) T.lift -> ('a option, 'res) T.lift
    method list      : 'a. ('a,    'res) T.lift -> ('a list, 'res) T.lift
    method position : (Astlib.Position.t, 'res) T.lift
    method location : (Astlib.Location.t, 'res) T.lift
    method loc : 'a. ('a, 'res) T.lift -> ('a Astlib.Loc.t, 'res) T.lift
  end
