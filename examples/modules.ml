(*
  modules.ml
  contains various pathological modules syntax to test ocaml+twt. does nothing useful
*)

module A = struct
 type point = { x : int; y : int}
 let origin = { x = 0; y = 0 }

 module B =
 struct
   let foo = 123

 module type tywithinty = sig
  val foo : int

 class type classwithinty =
  object
    method x : int


module C =
 struct
   let foo = 123

module type At = sig
  type point = { x : int; y : int}
  val origin : point

  module B : sig
   val foo : int

  module type tywithinty = sig
   val foo : int

  class type classwithinty =
   object
     method x : int

let f () =
 (* local modules: the struct has to start on its own line. You cannot declare a local module at the top level (that is, the following only works within f()) *)
 let module M =
  struct
   let k = 12345 
 M.k


(* functors: "module Name = functor ... ->" must be entirely on one line *)
module D = functor (A : At) ->
struct
  type f = int -> A.classwithinty
  let x = 1

module E = D(A)

