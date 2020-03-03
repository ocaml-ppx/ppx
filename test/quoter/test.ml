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
[%%expect{|
val expr1 : expression =
  {Ppx__.Import.pexp_desc =
    Ppx__.Import.Pexp_ident
     {Ppx__.Import.txt = Ppx__.Import.Lident "__0";
      loc =
       {Ppx__.Import.loc_start =
         {Ppx__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
          pos_cnum = -1};
        loc_end =
         {Ppx__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
          pos_cnum = -1};
        loc_ghost = true}};
   pexp_loc =
    {Ppx__.Import.loc_start =
      {Ppx__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
       pos_cnum = -1};
     loc_end =
      {Ppx__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
       pos_cnum = -1};
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
[%%expect{|
val expr2 : expression =
  {Ppx__.Import.pexp_desc =
    Ppx__.Import.Pexp_ident
     {Ppx__.Import.txt = Ppx__.Import.Lident "__1";
      loc =
       {Ppx__.Import.loc_start =
         {Ppx__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
          pos_cnum = -1};
        loc_end =
         {Ppx__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
          pos_cnum = -1};
        loc_ghost = true}};
   pexp_loc =
    {Ppx__.Import.loc_start =
      {Ppx__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
       pos_cnum = -1};
     loc_end =
      {Ppx__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
       pos_cnum = -1};
     loc_ghost = true};
   pexp_attributes = []}
|}]

let quoted =
  let expr = Ast.elist ~loc:Location.none [expr1; expr2] in
  Quoter.sanitize quoter expr
[%%expect{|
val quoted : expression =
  {Ppx__.Import.pexp_desc =
    Ppx__.Import.Pexp_let (Ppx__.Import.Recursive,
     [{Ppx__.Import.pvb_pat =
        {Ppx__.Import.ppat_desc =
          Ppx__.Import.Ppat_var
           {Ppx__.Import.txt = "__1";
            loc =
             {Ppx__.Import.loc_start =
               {Ppx__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
                pos_cnum = -1};
              loc_end =
               {Ppx__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
                pos_cnum = -1};
              loc_ghost = true}};
         ppat_loc =
          {Ppx__.Import.loc_start =
            {Ppx__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
             pos_cnum = -1};
           loc_end =
            {Ppx__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
             pos_cnum = -1};
           loc_ghost = true};
         ppat_attributes = []};
       pvb_expr =
        {Ppx__.Import.pexp_desc =
          Ppx__.Import.Pexp_fun (Ppx__.Import.Nolabel, None,
           {Ppx__.Import.ppat_desc =
             Ppx__.Import.Ppat_construct
              ({Ppx__.Import.txt = Ppx__.Import.Lident "()";
                loc =
                 {Ppx__.Import.loc_start =
                   {Ppx__.Import.pos_fname = "_none_"; pos_lnum = 1;
                    pos_bol = 0; pos_cnum = -1};
                  loc_end =
                   {Ppx__.Import.pos_fname = "_none_"; pos_lnum = 1;
                    pos_bol = 0; pos_cnum = -1};
                  loc_ghost = true}},
              None);
            ppat_loc =
             {Ppx__.Import.loc_start =
               {Ppx__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
                pos_cnum = -1};
              loc_end =
               {Ppx__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
                pos_cnum = -1};
              loc_ghost = true};
            ppat_attributes = []},
           {Ppx__.Import.pexp_desc =
             Ppx__.Import.Pexp_ident
              {Ppx__.Import.txt = Ppx__.Import.Lident "bar";
               loc =
                {Ppx__.Import.loc_start =
                  {Ppx__.Import.pos_fname = "_none_"; pos_lnum = 1;
                   pos_bol = 0; pos_cnum = -1};
                 loc_end =
                  {Ppx__.Import.pos_fname = "_none_"; pos_lnum = 1;
                   pos_bol = 0; pos_cnum = -1};
                 loc_ghost = true}};
            pexp_loc =
             {Ppx__.Import.loc_start =
               {Ppx__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
                pos_cnum = -1};
              loc_end =
               {Ppx__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
                pos_cnum = -1};
              loc_ghost = true};
            pexp_attributes = []});
         pexp_loc =
          {Ppx__.Import.loc_start =
            {Ppx__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
             pos_cnum = -1};
           loc_end =
            {Ppx__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
             pos_cnum = -1};
           loc_ghost = true};
         pexp_attributes = []};
       pvb_attributes = [];
       pvb_loc =
        {Ppx__.Import.loc_start =
          {Ppx__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
           pos_cnum = -1};
         loc_end =
          {Ppx__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
           pos_cnum = -1};
         loc_ghost = true}};
      {Ppx__.Import.pvb_pat =
        {Ppx__.Import.ppat_desc =
          Ppx__.Import.Ppat_var
           {Ppx__.Import.txt = "__0";
            loc =
             {Ppx__.Import.loc_start =
               {Ppx__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
                pos_cnum = -1};
              loc_end =
               {Ppx__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
                pos_cnum = -1};
              loc_ghost = true}};
         ppat_loc =
          {Ppx__.Import.loc_start =
            {Ppx__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
             pos_cnum = -1};
           loc_end =
            {Ppx__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
             pos_cnum = -1};
           loc_ghost = true};
         ppat_attributes = []};
       pvb_expr =
        {Ppx__.Import.pexp_desc =
          Ppx__.Import.Pexp_fun (Ppx__.Import.Nolabel, None,
           {Ppx__.Import.ppat_desc =
             Ppx__.Import.Ppat_construct
              ({Ppx__.Import.txt = Ppx__.Import.Lident "()";
                loc =
                 {Ppx__.Import.loc_start =
                   {Ppx__.Import.pos_fname = "_none_"; pos_lnum = 1;
                    pos_bol = 0; pos_cnum = -1};
                  loc_end =
                   {Ppx__.Import.pos_fname = "_none_"; pos_lnum = 1;
                    pos_bol = 0; pos_cnum = -1};
                  loc_ghost = true}},
              None);
            ppat_loc =
             {Ppx__.Import.loc_start =
               {Ppx__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
                pos_cnum = -1};
              loc_end =
               {Ppx__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
                pos_cnum = -1};
              loc_ghost = true};
            ppat_attributes = []},
           {Ppx__.Import.pexp_desc =
             Ppx__.Import.Pexp_ident
              {Ppx__.Import.txt = Ppx__.Import.Lident "foo";
               loc =
                {Ppx__.Import.loc_start =
                  {Ppx__.Import.pos_fname = "_none_"; pos_lnum = 1;
                   pos_bol = 0; pos_cnum = -1};
                 loc_end =
                  {Ppx__.Import.pos_fname = "_none_"; pos_lnum = 1;
                   pos_bol = 0; pos_cnum = -1};
                 loc_ghost = true}};
            pexp_loc =
             {Ppx__.Import.loc_start =
               {Ppx__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
                pos_cnum = -1};
              loc_end =
               {Ppx__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
                pos_cnum = -1};
              loc_ghost = true};
            pexp_attributes = []});
         pexp_loc =
          {Ppx__.Import.loc_start =
            {Ppx__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
             pos_cnum = -1};
           loc_end =
            {Ppx__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
             pos_cnum = -1};
           loc_ghost = true};
         pexp_attributes = []};
       pvb_attributes = [];
       pvb_loc =
        {Ppx__.Import.loc_start =
          {Ppx__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
           pos_cnum = -1};
         loc_end =
          {Ppx__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
           pos_cnum = -1};
         loc_ghost = true}}],
     {Ppx__.Import.pexp_desc =
       Ppx__.Import.Pexp_construct
        ({Ppx__.Import.txt = Ppx__.Import.Lident "::";
          loc =
           {Ppx__.Import.loc_start =
             {Ppx__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
              pos_cnum = -1};
            loc_end =
             {Ppx__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
              pos_cnum = -1};
            loc_ghost = true}},
        Some
         {Ppx__.Import.pexp_desc =
           Ppx__.Import.Pexp_tuple
            [{Ppx__.Import.pexp_desc =
               Ppx__.Import.Pexp_ident
                {Ppx__.Import.txt = Ppx__.Import.Lident "__0";
                 loc =
                  {Ppx__.Import.loc_start =
                    {Ppx__.Import.pos_fname = "_none_"; pos_lnum = 1;
                     pos_bol = 0; pos_cnum = -1};
                   loc_end =
                    {Ppx__.Import.pos_fname =
                      "_non"... (* string length 6; truncated *);
                     pos_lnum = 1; pos_bol = 0; pos_cnum = -1};
                   loc_ghost = true}};
              pexp_loc = ...; pexp_attributes = ...};
             ...];
          pexp_loc = ...; pexp_attributes = ...});
      pexp_loc = ...; pexp_attributes = ...});
   pexp_loc = ...; pexp_attributes = ...}
|}]

Pprintast.string_of_expression quoted;;
[%%expect{|
- : string = "let rec __1 () = bar\nand __0 () = foo in [__0; __1]"
|}]
