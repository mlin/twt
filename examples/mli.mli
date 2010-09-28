module A : sig
  val x : int

  class type c = object
    method z : int -> int

type t =
  | Yes
  | No

val not : t -> t

class type iwindow = object
  method top_widget : iwidget
and iwidget = object
  method window : iwindow
