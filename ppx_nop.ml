open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

let nop_mapper argv =
  { default_mapper with
    expr = fun mapper expr ->
      match expr with
      | { pexp_desc =
          Pexp_extension ({ txt = "nop"; loc }, pstr)} -> Exp.constant ~loc (Const_int 0)
      | x -> default_mapper.expr mapper x;
  }

let () = register "nop" nop_mapper
