if (Array.length Sys.argv) < 3 then
 Printf.eprintf "Usage: ppcompose [-v] pp1 pp2 ... source.ml\n"
 Printf.eprintf " The preprocessors are composed in the order (pp1 (pp2 source.ml))\n"
 Printf.eprintf " If a preprecossor command has arguments, put it in quotes.\n"
 Printf.eprintf " examples:\n"
 Printf.eprintf "  ppcompose \"camlp4o pa_compr.cmo\" ocaml+twt source.ml\n"
 Printf.eprintf "  ocamlc -pp \"ppcompose 'camlp4o pa_compr.cmo' 'ocaml+twt -spaceonly'\" -c source.ml\n"
 exit 2

let verbose = ref false
let pps = ref []
for i = 1 to (Array.length Sys.argv) - 2 do
 if Sys.argv.(i) = "-v" then
  verbose := true
 else
  pps := Sys.argv.(i) :: !pps
let verbose = !verbose
let pps = List.rev !pps
let source_fname = Sys.argv.((Array.length Sys.argv) - 1)

let temp_fns = ref []
at_exit
  fun () ->
    List.iter
      fun fn ->
        try
          Sys.remove fn
          if verbose then
            Printf.eprintf "removed %s\n" fn
            flush stderr
        with
          | exn -> Printf.eprintf "while trying to remove %s: %s\n" fn (Printexc.to_string exn)
      !temp_fns

let apply_pp pp input_fname =
  let without_extension =
    try
      Filename.chop_extension input_fname
    with
      | Invalid_argument _ -> input_fname
  let extension =
    try
      String.sub
        input_fname
        String.length without_extension
        (String.length input_fname) - (String.length without_extension)
    with
      | Invalid_argument _ -> ""
  let output_fname = Filename.temp_file "ppcompose" extension
  temp_fns := output_fname :: !temp_fns
  let cmd = Printf.sprintf "%s %s > %s" pp input_fname output_fname
  if verbose then
    Printf.eprintf "%s\n" cmd
    flush stderr
  match Sys.command cmd with
    | 0 -> output_fname
    | _ as c -> exit c

let apply_outermost_pp pp input_fname =
  let cmd = Printf.sprintf "%s %s" pp input_fname
  if verbose then
    Printf.eprintf "%s\n" cmd
    flush stderr
  match Sys.command cmd with
    | 0 -> ()
    | _ as c -> exit c

let rec apply_pps = function
  | [] -> source_fname
  | innermost :: [] -> apply_pp innermost source_fname
  | some_pp :: rest ->
      let input_fname = apply_pps rest
      apply_pp some_pp input_fname

let output_fname = apply_pps (List.tl pps)
apply_outermost_pp (List.hd pps) output_fname

