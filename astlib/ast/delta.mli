type ('key, 'insert, 'modify) edit =
  | Remove of 'key
  | Insert of int * 'insert
  | Modify of 'key * 'modify

type ('insert, 'modify) alist = (string, string * 'insert, 'modify) edit list

type structural = Grammar.structural

type tuple = (int, Grammar.structural, structural) edit list

type record = (Grammar.structural, structural) alist

type clause =
  | Tuple of tuple
  | Record of record

type variant = (Grammar.clause, clause) alist

type nominal =
  | Alias of structural
  | Record of record
  | Variant of variant

type grammar = (Grammar.decl, nominal) alist

val apply_to_grammar : grammar -> Grammar.t -> Grammar.t
