#require "base";;
#require "ppx.metaquot_lifters";;
#require "ppx.ast_deprecated";;

open Ppx

module Ast = Ast_builder
[%%expect{|
module Ast = Ppx.Ast_builder
|}]

let quoter = Quoter.create ();;
[%%expect{|
val quoter : Quoter.t = <abstr>
|}]

#install_printer Pprintast.expression;;

let expr1 =
  Ast.evar "foo" ~loc:Location.none
  |> Quoter.quote quoter
  |> Conversion.ast_to_expression
[%%expect{|
val expr1 : Parsetree.expression =
  {Ppx_ast__.Compiler_types.pexp_desc =
    Ppx_ast__.Compiler_types.Pexp_ident
     {Asttypes.txt = Longident.Lident "__0";
      loc =
       {Location.loc_start =
         {Lexing.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
          pos_cnum = -1};
        loc_end =
         {Lexing.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
          pos_cnum = -1};
        loc_ghost = true}};
   pexp_loc =
    {Location.loc_start =
      {Lexing.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0; pos_cnum = -1};
     loc_end =
      {Lexing.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0; pos_cnum = -1};
     loc_ghost = true};
   pexp_attributes = []}
|}]

Pprintast.string_of_expression expr1;;
[%%expect{|
- : string = "__0"
|}]

let expr2 =
  Ast_builder.evar ~loc:Location.none "bar"
  |> Quoter.quote quoter
  |> Conversion.ast_to_expression
[%%expect{|
val expr2 : Parsetree.expression =
  {Ppx_ast__.Compiler_types.pexp_desc =
    Ppx_ast__.Compiler_types.Pexp_ident
     {Asttypes.txt = Longident.Lident "__1";
      loc =
       {Location.loc_start =
         {Lexing.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
          pos_cnum = -1};
        loc_end =
         {Lexing.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
          pos_cnum = -1};
        loc_ghost = true}};
   pexp_loc =
    {Location.loc_start =
      {Lexing.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0; pos_cnum = -1};
     loc_end =
      {Lexing.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0; pos_cnum = -1};
     loc_ghost = true};
   pexp_attributes = []}
|}]

let quoted =
  let expr =
    Ast.elist ~loc:Location.none [
      Conversion.ast_of_expression expr1;
      Conversion.ast_of_expression expr2;
    ]
  in
  Quoter.sanitize quoter expr
  |> Conversion.ast_to_expression
[%%expect{|
val quoted : Parsetree.expression =
  {Ppx_ast__.Compiler_types.pexp_desc =
    Ppx_ast__.Compiler_types.Pexp_let (Asttypes.Recursive,
     [{Ppx_ast__.Compiler_types.pvb_pat =
        {Ppx_ast__.Compiler_types.ppat_desc =
          Ppx_ast__.Compiler_types.Ppat_var
           {Asttypes.txt = "__1";
            loc =
             {Location.loc_start =
               {Lexing.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
                pos_cnum = -1};
              loc_end =
               {Lexing.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
                pos_cnum = -1};
              loc_ghost = true}};
         ppat_loc =
          {Location.loc_start =
            {Lexing.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
             pos_cnum = -1};
           loc_end =
            {Lexing.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
             pos_cnum = -1};
           loc_ghost = true};
         ppat_attributes = []};
       pvb_expr =
        {Ppx_ast__.Compiler_types.pexp_desc =
          Ppx_ast__.Compiler_types.Pexp_fun (Asttypes.Nolabel, None,
           {Ppx_ast__.Compiler_types.ppat_desc =
             Ppx_ast__.Compiler_types.Ppat_construct
              ({Asttypes.txt = Longident.Lident "()";
                loc =
                 {Location.loc_start =
                   {Lexing.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
                    pos_cnum = -1};
                  loc_end =
                   {Lexing.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
                    pos_cnum = -1};
                  loc_ghost = true}},
              None);
            ppat_loc =
             {Location.loc_start =
               {Lexing.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
                pos_cnum = -1};
              loc_end =
               {Lexing.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
                pos_cnum = -1};
              loc_ghost = true};
            ppat_attributes = []},
           {Ppx_ast__.Compiler_types.pexp_desc =
             Ppx_ast__.Compiler_types.Pexp_ident
              {Asttypes.txt = Longident.Lident "bar";
               loc =
                {Location.loc_start =
                  {Lexing.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
                   pos_cnum = -1};
                 loc_end =
                  {Lexing.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
                   pos_cnum = -1};
                 loc_ghost = true}};
            pexp_loc =
             {Location.loc_start =
               {Lexing.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
                pos_cnum = -1};
              loc_end =
               {Lexing.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
                pos_cnum = -1};
              loc_ghost = true};
            pexp_attributes = []});
         pexp_loc =
          {Location.loc_start =
            {Lexing.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
             pos_cnum = -1};
           loc_end =
            {Lexing.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
             pos_cnum = -1};
           loc_ghost = true};
         pexp_attributes = []};
       pvb_attributes = [];
       pvb_loc =
        {Location.loc_start =
          {Lexing.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
           pos_cnum = -1};
         loc_end =
          {Lexing.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
           pos_cnum = -1};
         loc_ghost = true}};
      {Ppx_ast__.Compiler_types.pvb_pat =
        {Ppx_ast__.Compiler_types.ppat_desc =
          Ppx_ast__.Compiler_types.Ppat_var
           {Asttypes.txt = "__0";
            loc =
             {Location.loc_start =
               {Lexing.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
                pos_cnum = -1};
              loc_end =
               {Lexing.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
                pos_cnum = -1};
              loc_ghost = true}};
         ppat_loc =
          {Location.loc_start =
            {Lexing.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
             pos_cnum = -1};
           loc_end =
            {Lexing.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
             pos_cnum = -1};
           loc_ghost = true};
         ppat_attributes = []};
       pvb_expr =
        {Ppx_ast__.Compiler_types.pexp_desc =
          Ppx_ast__.Compiler_types.Pexp_fun (Asttypes.Nolabel, None,
           {Ppx_ast__.Compiler_types.ppat_desc =
             Ppx_ast__.Compiler_types.Ppat_construct
              ({Asttypes.txt = Longident.Lident "()";
                loc =
                 {Location.loc_start =
                   {Lexing.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
                    pos_cnum = -1};
                  loc_end =
                   {Lexing.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
                    pos_cnum = -1};
                  loc_ghost = true}},
              None);
            ppat_loc =
             {Location.loc_start =
               {Lexing.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
                pos_cnum = -1};
              loc_end =
               {Lexing.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
                pos_cnum = -1};
              loc_ghost = true};
            ppat_attributes = []},
           {Ppx_ast__.Compiler_types.pexp_desc =
             Ppx_ast__.Compiler_types.Pexp_ident
              {Asttypes.txt = Longident.Lident "foo";
               loc =
                {Location.loc_start =
                  {Lexing.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
                   pos_cnum = -1};
                 loc_end =
                  {Lexing.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
                   pos_cnum = -1};
                 loc_ghost = true}};
            pexp_loc =
             {Location.loc_start =
               {Lexing.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
                pos_cnum = -1};
              loc_end =
               {Lexing.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
                pos_cnum = -1};
              loc_ghost = true};
            pexp_attributes = []});
         pexp_loc =
          {Location.loc_start =
            {Lexing.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
             pos_cnum = -1};
           loc_end =
            {Lexing.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
             pos_cnum = -1};
           loc_ghost = true};
         pexp_attributes = []};
       pvb_attributes = [];
       pvb_loc =
        {Location.loc_start =
          {Lexing.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
           pos_cnum = -1};
         loc_end =
          {Lexing.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
           pos_cnum = -1};
         loc_ghost = true}}],
     {Ppx_ast__.Compiler_types.pexp_desc =
       Ppx_ast__.Compiler_types.Pexp_construct
        ({Asttypes.txt = Longident.Lident "::";
          loc =
           {Location.loc_start =
             {Lexing.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
              pos_cnum = -1};
            loc_end =
             {Lexing.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
              pos_cnum = -1};
            loc_ghost = true}},
        Some
         {Ppx_ast__.Compiler_types.pexp_desc =
           Ppx_ast__.Compiler_types.Pexp_tuple
            [{Ppx_ast__.Compiler_types.pexp_desc =
               Ppx_ast__.Compiler_types.Pexp_ident
                {Asttypes.txt = Longident.Lident "__0";
                 loc =
                  {Location.loc_start =
                    {Lexing.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
                     pos_cnum = -1};
                   loc_end =
                    {Lexing.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
                     pos_cnum = -1};
                   loc_ghost = true}};
              pexp_loc =
               {Location.loc_start =
                 {Lexing.pos_fname =
                   "_no"... (* string length 6; truncated *);
                  pos_lnum = 1; pos_bol = 0; pos_cnum = -1};
                loc_end = ...; loc_ghost = ...};
              pexp_attributes = ...};
             ...];
          pexp_loc = ...; pexp_attributes = ...});
      pexp_loc = ...; pexp_attributes = ...});
   pexp_loc = ...; pexp_attributes = ...}
|}]

Pprintast.string_of_expression quoted;;
[%%expect{|
- : string = "let rec __1 () = bar\nand __0 () = foo in [__0; __1]"
|}]
