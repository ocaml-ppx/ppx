let (v3_grammar : Grammar.t) =
  [ { kind_name = "Identifier"
    ; clauses =
        [ { clause_name = "Var"
          ; fields = [ { field_name = "name"; data = String } ]
          }
        ; { clause_name = "Dot"
          ; fields =
              [ { field_name = "base"; data = Kind "Identifier" }
              ; { field_name = "label"; data = String }
              ]
          }
        ]
    }
  ; { kind_name = "Expression"
    ; clauses =
        [ { clause_name = "Ident"
          ; fields = [ { field_name = "id"; data = Kind "Identifier" } ]
          }
        ; { clause_name = "Match"
          ; fields =
              [ { field_name = "arg"; data = Kind "Expression" }
              ; { field_name = "cases"; data = List (Kind "Case") }
              ]
          }
        ]
    }
  ; { kind_name = "Case"
    ; clauses =
        [ { clause_name = "Case"
          ; fields =
              [ { field_name = "lhs"; data = Kind "Pattern" }
              ; { field_name = "rhs"; data = Kind "Expression" }
              ]
          }
        ]
    }
  ; { kind_name = "Pattern"
    ; clauses =
        [ { clause_name = "Ident"
          ; fields = [ { field_name = "name"; data = String } ]
          }
        ; { clause_name = "Guard"
          ; fields =
              [ { field_name = "body"; data = Kind "Pattern" }
              ; { field_name = "guard"; data = Kind "Expression" }
              ]
          }
        ]
    }
  ]

let v3_of_v2 : History.conversion =
  { name = "v3_of_v2"
  ; f = fun node ~to_node:_ ~of_node ->
      match node with
      | { kind = "Expression"
        ; clause = "Ident"
        ; loc
        ; fields = [ { name = "name"; value = name } ]
        } ->
        { kind = "Expression"
        ; clause = "Ident"
        ; loc
        ; fields =
            [ { name = "id"
              ; value =
                  Tree
                    (of_node
                       { kind = "Identifier"
                       ; clause = "Var"
                       ; loc
                       ; fields = [ { name = "name"; value = name } ]
                       })
              }
            ]
        }
      | _ -> node
  }

let v2_of_v3 : History.conversion =
  { name = "v2_of_v3"
  ; f = fun node ~to_node ~of_node:_ ->
      match node with
      | { kind = "Expression"
        ; clause = "Ident"
        ; loc
        ; fields = [ { name = "id" ; value = Tree id } ]
        }
        ->
        (match to_node id with
         | { kind = "Identifier"
           ; clause = "Var"
           ; loc = _
           ; fields = [ { name = "name"; value = name } ]
           }
           ->
           { kind = "Expression"
           ; clause = "Ident"
           ; loc
           ; fields = [ { name = "name"; value = name } ]
           }
         | _ -> node)
      | _ -> node
  }

let (v2_delta_from_v3 : Delta.grammar) =
  [ Remove "Identifier"
  ; Modify
      ("Expression",
       [ Modify
           ("Ident", [ Remove "id"; Insert (0, { field_name = "name"; data = String }) ])
       ])
  ]

let (v2 : History.previous_version) =
  { version = "V2"
  ; next_version = "V3"
  ; delta_from_next = v2_delta_from_v3
  ; to_next = v3_of_v2
  ; of_next = v2_of_v3
  }

let v2_of_v1 : History.conversion =
  { name = "v2_of_v1"
  ; f = fun node ~to_node:_ ~of_node ->
      match node with
      | { kind = "Case"
        ; clause = "Case"
        ; loc
        ; fields =
            [ { name = "lhs"; value = lhs }
            ; { name = "guard"; value = Option guard }
            ; { name = "rhs"; value = rhs }
            ]
        }
        ->
        let lhs =
          match guard with
          | None -> lhs
          | Some guard ->
            Tree
              (of_node
                 { kind = "Pattern"
                 ; clause = "Guard"
                 ; loc
                 ; fields =
                     [ { name = "body"; value = lhs }
                     ; { name = "guard"; value = guard }
                     ]
                 })
        in
        { kind = "Case"
        ; clause = "Case"
        ; loc
        ; fields =
            [ { name = "lhs"; value = lhs }
            ; { name = "rhs"; value = rhs }
            ]
        }
      | _ -> node
  }

let v1_of_v2 : History.conversion =
  { name = "v1_of_v2"
  ; f = fun node ~to_node ~of_node:_ ->
      match node with
      | { kind = "Case"
        ; clause = "Case"
        ; loc
        ; fields =
            [ { name = "lhs"; value = Tree lhs }
            ; { name = "rhs"; value = rhs }
            ]
        }
        ->
        let lhs, guard =
          match to_node lhs with
          | { kind = "Pattern"
            ; clause = "Guard"
            ; loc = _
            ; fields =
                [ { name = "body"; value = lhs }
                ; { name = "guard"; value = guard }
                ]
            }
            ->
            lhs, Some guard
          | _ ->
            Tree lhs, None
        in
        { kind = "Case"
        ; clause = "Case"
        ; loc
        ; fields =
            [ { name = "lhs"; value = lhs }
            ; { name = "guard"; value = Option guard }
            ; { name = "rhs"; value = rhs }
            ]
        }
      | _ -> node
  }

let (v1_delta_from_v2 : Delta.grammar) =
  [ Modify
      ("Case",
       [ Modify
           ("Case",
            [ Insert (1, { field_name = "guard"; data = Option (Kind "Expression") }) ])
       ])
  ; Modify ("Pattern", [ Remove "Guard" ])
  ]

let (v1 : History.previous_version) =
  { version = "V1"
  ; next_version = "V2"
  ; delta_from_next = v1_delta_from_v2
  ; to_next = v2_of_v1
  ; of_next = v1_of_v2
  }

let history =
  History.create
    ~current_version:"V3"
    ~current_grammar:v3_grammar
    ~previous_versions:[v1; v2]
