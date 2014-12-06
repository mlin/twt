(*
  nussinov.ml
  implements the simple Nussinov algorithm for RNA folding,
  and evaluates it on random sequence
*)

let rec integers a b =
  if a <= b then
    a :: (integers (a + 1) b)
  else
    []

let s = function
  | ('A','U')
  | ('G','C')
  | ('C','G')
  | ('U','A') -> 1.0
  | ('G','U')
  | ('U','G') -> 0.5
  | _ -> 0.0

let list_max lst =
  List.fold_left max (List.hd lst) (List.tl lst)

let rec make_f seq =
  let rec f (i,j) =
    if i < j then
      let ks = integers i (j - 1)
      let f_ikjs = List.map (fun k -> (mf (i,k)) +. (mf (k+1,j))) ks
      let ij = (s (seq.[i],seq.[j])) +. (mf (i+1,j-1))
      list_max (ij :: f_ikjs)
    else 0.0
  and mf = (* memoize f *)
    let t = Hashtbl.create 10
    fun x ->
      try
        Hashtbl.find t x
      with
        | Not_found ->
          let y = f x
          Hashtbl.add t x y
          y
  mf

(* random sequence generation *)

let random_base gc_content =
  let cum_p =
    [| gc_content /. 2.0;
       gc_content;
       gc_content +. (1.0 -. gc_content) /. 2.0;
       1.0 |]
  let x = Random.float 1.0
  let i = ref 0
  while cum_p.(!i) < x do i := !i + 1
  [| 'G'; 'C'; 'A'; 'U' |].(!i)

let random_seq gc_content len =
  let s = Bytes.create len
  for i = 0 to len - 1 do
    Bytes.set s i (random_base gc_content)
  Bytes.to_string s

(* test trials *)

let trial nseqs seqlen gc_content =
  let seqs = List.map (fun _ -> random_seq gc_content seqlen) (integers 1 nseqs)
  let scores = List.map (fun seq -> ((make_f seq) (0,seqlen-1))) seqs
  (/.)
    List.fold_left (+.) 0.0 scores
    float_of_int nseqs

let ptrial nseqs seqlen gc_content =
  let avg_score = trial nseqs seqlen gc_content
  Printf.printf "%d\t%d\t%f\t%f\n" nseqs seqlen gc_content avg_score
  flush_all ()

Random.self_init ()

ptrial 100 100 0.5
ptrial 100 100 0.25
ptrial 100 100 0.75
ptrial 100 100 0.10
ptrial 100 100 0.90