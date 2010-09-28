let rec fib = function
  | 0 | 1 -> 1
  | n when n > 1 -> (fibm (n - 2)) + (fibm (n - 1))
and fibm =
  let tbl = Hashtbl.create 10
  fun x ->
   try
    Hashtbl.find tbl x
   with
     | Not_found ->
         let y = fib x
         Hashtbl.add tbl x y
         y

for i = 0 to 20 do
  Printf.printf "%d\n" (fib i)
