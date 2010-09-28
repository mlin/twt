(*
  objects.ml
  contains various object syntax forms to test ocaml+twt
*)

module Shape = struct
  class virtual t =
  object(self)
    method virtual area : unit -> float
    method to_string () =
     "shape with area " ^ (string_of_float (self#area ()))
    initializer
     print_endline "created a shape"

  class circle radius =
    let pi = 3.14159
    object
     inherit t
     method area () =
      pi *. radius *. radius

  class square sidelen = object
    inherit t
    method area () =
     sidelen *. sidelen

let c = new Shape.circle 2.0
let s = new Shape.square 4.0

print_endline (c#to_string ())
print_endline (s#to_string ())

class type to_string_able = object
  val to_string : unit -> string

ignore (s :> to_string_able)

class window = object
  val mutable top_widget = (None : widget option)
  method top_widget = top_widget
and widget (w : window) = object
  val window = w
  method window = window

class window2 =
  object
    val mutable top_widget = (None : widget2 option)
    method top_widget = top_widget
and widget2 (w : window2) = 
 let pi = 3.14159
 object
  val window = w
  method window = window

class type iwindow = object
  method top_widget : iwidget
and iwidget = object
  method window : iwindow
