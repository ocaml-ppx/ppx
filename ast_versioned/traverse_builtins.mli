module T : sig
  type 'a map = 'a -> 'a
  type 'a iter = 'a -> unit
  type ('a, 'acc) fold = 'a -> 'acc -> 'acc
  type ('a, 'acc) fold_map = 'a -> 'acc -> ('a * 'acc)
  type ('ctx, 'a) map_with_context = 'ctx -> 'a -> 'a
  type ('a, 'res) lift = 'a -> 'res
end

class map :
  object
    method int    : int    T.map
    method string : string T.map
    method bool   : bool   T.map
    method char   : char   T.map
    method option : 'a. 'a T.map -> 'a option T.map
    method list : 'a. 'a T.map -> 'a list T.map
    method array : 'a. 'a T.map -> 'a array T.map
    method position : Astlib.Position.t T.map
    method location : Astlib.Location.t T.map
    method loc : 'a. 'a T.map -> 'a Astlib.Loc.t T.map
  end

class iter :
  object
    method int    : int    T.iter
    method string : string T.iter
    method bool   : bool   T.iter
    method char   : char   T.iter
    method option : 'a. 'a T.iter -> 'a option T.iter
    method list : 'a. 'a T.iter -> 'a list T.iter
    method array : 'a. 'a T.iter -> 'a array T.iter
    method position : Astlib.Position.t T.iter
    method location : Astlib.Location.t T.iter
    method loc : 'a. 'a T.iter -> 'a Astlib.Loc.t T.iter
  end

class ['acc] fold :
  object
    method int    : (int    , 'acc) T.fold
    method string : (string , 'acc) T.fold
    method bool   : (bool   , 'acc) T.fold
    method char   : (char   , 'acc) T.fold
    method option : 'a. ('a, 'acc) T.fold -> ('a option, 'acc) T.fold
    method list : 'a. ('a, 'acc) T.fold -> ('a list, 'acc) T.fold
    method array : 'a. ('a, 'acc) T.fold -> ('a array, 'acc) T.fold
    method position : (Astlib.Position.t, 'acc) T.fold
    method location : (Astlib.Location.t, 'acc) T.fold
    method loc : 'a. ('a, 'acc) T.fold -> ('a Astlib.Loc.t, 'acc) T.fold
  end

class ['acc] fold_map :
  object
    method int    : (int    , 'acc) T.fold_map
    method string : (string , 'acc) T.fold_map
    method bool   : (bool   , 'acc) T.fold_map
    method char   : (char   , 'acc) T.fold_map
    method option : 'a. ('a, 'acc) T.fold_map -> ('a option, 'acc) T.fold_map
    method list : 'a. ('a, 'acc) T.fold_map -> ('a list, 'acc) T.fold_map
    method array : 'a. ('a, 'acc) T.fold_map -> ('a array, 'acc) T.fold_map
    method position : (Astlib.Position.t, 'acc) T.fold_map
    method location : (Astlib.Location.t, 'acc) T.fold_map
    method loc : 'a. ('a, 'acc) T.fold_map -> ('a Astlib.Loc.t, 'acc) T.fold_map
  end

class ['ctx] map_with_context :
  object
    method int    : ('ctx, int   ) T.map_with_context
    method string : ('ctx, string) T.map_with_context
    method bool   : ('ctx, bool  ) T.map_with_context
    method char   : ('ctx, char  ) T.map_with_context
    method option
      : 'a. ('ctx, 'a) T.map_with_context -> ('ctx, 'a option) T.map_with_context
    method list
      : 'a. ('ctx, 'a) T.map_with_context -> ('ctx, 'a list) T.map_with_context
    method array
      : 'a. ('ctx, 'a) T.map_with_context -> ('ctx, 'a array) T.map_with_context
    method position : ('ctx, Astlib.Position.t) T.map_with_context
    method location : ('ctx, Astlib.Location.t) T.map_with_context
    method loc
      : 'a. ('ctx, 'a) T.map_with_context -> ('ctx, 'a Astlib.Loc.t) T.map_with_context
  end

class virtual ['res] lift :
  object
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
    method virtual record : (string * 'res) list -> 'res
    method virtual constr : string -> 'res list -> 'res
    method virtual tuple : 'res list -> 'res
    method option : 'a. ('a, 'res) T.lift -> ('a option, 'res) T.lift
    method list : 'a. ('a, 'res) T.lift -> ('a list, 'res) T.lift
    method position : (Astlib.Position.t, 'res) T.lift
    method location : (Astlib.Location.t, 'res) T.lift
    method loc : 'a. ('a, 'res) T.lift -> ('a Astlib.Loc.t, 'res) T.lift
  end

class type ['res] std_lifters =
  object
    method other     : 'a. ('a,    'res) T.lift
    method int       : (int ,      'res) T.lift
    method string    : (string,    'res) T.lift
    method bool      : (bool ,     'res) T.lift
    method char      : (char ,     'res) T.lift
    method array     : 'a. ('a,    'res) T.lift -> ('a array, 'res) T.lift
    method record    : (string *   'res) list -> 'res
    method constr    : string ->   'res  list -> 'res
    method tuple     : 'res list   ->    'res
    method float     : (float,     'res) T.lift
    method int32     : (int32,     'res) T.lift
    method int64     : (int64,     'res) T.lift
    method nativeint : (nativeint, 'res) T.lift
    method unit      : (unit,      'res) T.lift
    method option    : 'a. ('a,    'res) T.lift -> ('a option, 'res) T.lift
    method list      : 'a. ('a,    'res) T.lift -> ('a list, 'res) T.lift
  end
