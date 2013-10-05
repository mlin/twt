(*
  ocaml+twt.ml
  by Mike Lin (mikelin@mit.edu)

  "The Whitespace Thing" for OCaml

  This is a preprocessor for a new OCaml syntax that uses indentation rather than
  parenthesization to group expressions.
*)

type whitespace_mode =
    Either
  | Tab_only
  | Space_only

type configuration = {
  mutable whitespace_mode : whitespace_mode;
  mutable lwt : bool
}

let config = {
  whitespace_mode = Either;
  lwt = false
}

let endl = "\n"

(******************************************************************************
   stupid amateurish lexing stuff
 ******************************************************************************)

let whitespace_chars = [ ' '; '\t' ]
let comment_line_re = Str.regexp "[ \t]*(\\*.*"

let indent_count line =
  let i = ref 0 in
  let l = String.length line in
  while !i < l && List.mem line.[!i] whitespace_chars do
    i := !i + 1
  done;
  !i

let is_blank line =
  (indent_count line) = (String.length line) || (Str.string_match comment_line_re line 0) 

type lexical_state =
  {
    quote : bool;
    squote : bool;
    escape : bool;
    comment : int;
    paren : int;
    square : int;
    curly : int
  }

let update_lexical_state oldstate s =
  let quote = ref oldstate.quote in
  let squote = ref oldstate.squote in
  let escape = ref oldstate.escape in
  let comment = ref oldstate.comment in
  let paren = ref oldstate.paren in
  let square = ref oldstate.square in
  let curly = ref oldstate.curly in
  let inc x = x := 1 + !x in
  let dec x = x := !x - 1 in
  let len = String.length s in
  for i = 0 to len - 1 do
    let more = i < (len - 1) in
    let less = i > 0 in
    let litchar = less && s.[i-1] = '\'' && more && s.[i+1] = '\'' in
    match s.[i] with
    | _ when !escape -> escape := false
    | '\\' when (!quote || !squote) && not (i = len-1) -> escape := true
    | '"' when !comment = 0 && not !quote && not litchar -> quote := true
    | '"' when !quote && (not !escape) -> quote := false
    | '\'' when !comment = 0 && not !quote && not !squote && (not less || (List.mem s.[i-1] ('\r' :: '\n' :: whitespace_chars))) -> squote := true (* lousy hack to ignore identifiers with primes *)
    | '\'' when !squote && (not !escape) -> squote := false
    | '(' when more && s.[i+1] = '*' && not !quote -> inc comment
    | ')' when less && s.[i-1] = '*' && not !quote -> dec comment
    | '(' when !comment = 0 && not !quote && not litchar -> inc paren
    | ')' when !comment = 0 && not !quote && not litchar -> dec paren
    | '[' when !comment = 0 && not !quote && not litchar -> inc square
    | ']' when !comment = 0 && not !quote && not litchar -> dec square
    | '{' when !comment = 0 && not !quote && not litchar -> inc curly
    | '}' when !comment = 0 && not !quote && not litchar -> dec curly
    | _ -> ()
  done;
  { quote = !quote; squote = false; escape = !escape; comment = !comment;
    paren = max 0 !paren; square = !square; curly = !curly }

class line_reader chan =
  object(self)
    val mutable buf = None
    val mutable line_num = 0
    val mutable pre_lexical_state =
      {
        quote = false;
        squote = false;
        escape = false;
        comment = 0;
        paren = 0;
        square = 0;
        curly = 0
      }
    val mutable post_lexical_state =
      {
        quote = false;
        squote = false;
        escape = false;
        comment = 0;
        paren = 0;
        square = 0;
        curly = 0
      }

    method lexical_state () = pre_lexical_state
    method line_number () = line_num
    method peek () =
      match buf with
        Some line -> line
      | None ->
        let line = input_line chan in
        let post_state = update_lexical_state post_lexical_state line in
        pre_lexical_state <- post_lexical_state;
        post_lexical_state <- post_state;
        line_num <- 1 + line_num;
        buf <- Some line;
        line
    method read () =
      let line = self#peek () in
      buf <- None;
      line
    method drop () =
      ignore (self#read ())
  end

(******************************************************************************
   parsing
 ******************************************************************************)

type line_type =
  | Identifier
  | Paren
  | Curly
  | Square
  | Quote

  | NamedOperand
  | OptionalOperand

  | Let
  | In
  | And

  | If
  | Else

  | Fun

  | Match
  | Function
  | Pipe

  | For
  | While

  | Try
  | With

  | Open

  | Exception

  | Type
  | Val
  | Include

  | Module
  | ModuleType
  | Struct
  | Sig

  | Class
  | Object
  | Method
  | Initializer
  | Inherit
  | Constraint

let line_res =
  let keyword_spec = [
    ("let",Let);
    ("in",In);
    ("and",And);
    ("if",If);
    ("else",Else);
    ("for",For);
    ("while",While);
    ("match",Match);
    ("function",Function);
    ("try",Try);
    ("with",With);
    ("open",Open);
    ("open!",Open);
    ("exception",Exception);
    ("fun",Fun);
    ("type",Type);
    ("val",Val);
    ("val!",Val);
    ("include",Include);
    ("module[ \t]+type",ModuleType);
    ("module",Module);
    ("struct",Struct);
    ("sig",Sig);
    ("class",Class);
    ("object",Object);
    ("method",Method);
    ("method!",Method);
    ("initializer",Initializer);
    ("inherit",Inherit);
    ("inherit!",Inherit);
    ("constraint",Constraint)
  ] @ (if not config.lwt then [] else
         ["lwt", Let; "for_lwt", For; "while_lwt", While; "match_lwt", Match; "try_lwt", Try])
  in
  let keyword_res =
    List.map
      (fun (keyword,ty) -> (Str.regexp (keyword ^ "\\([ \t]+\\|$\\)"),ty))
      keyword_spec
  in
  keyword_res @
    [
      (Str.regexp "|.*",Pipe);
      (Str.regexp "(.*",Paren);
      (Str.regexp "{.*",Curly);
      (Str.regexp "\\[.*",Square);
      (Str.regexp "\".*",Quote);
      (Str.regexp "'.*",Quote);
      (Str.regexp "object(.*",Object);
      (Str.regexp "~[a-zA-Z0-9'_]+.*",NamedOperand);
      (Str.regexp "\\?[a-zA-Z0-9'_]+.*",OptionalOperand);
      (Str.regexp "[a-zA-Z0-9!`#].*",Identifier)
    ]

let determine_line_type line =
  let ic = indent_count line in
  let rec iter = function
      (re,ty) :: rest ->
      if Str.string_match re line ic then
        ty
      else
        iter rest
    | [] -> invalid_arg "determine_line_type"
  in
  iter line_res

(* a helpful warning *)
let check_indentation line_num line =
  let ic = indent_count line in
  if ic > 0 then
    let saw_space = ref false in
    let saw_tab = ref false in
    for i = 0 to ic - 1 do
      if line.[i] = ' ' then saw_space := true;
      if line.[i] = '\t' then saw_tab := true
    done;
    match config.whitespace_mode with
      Tab_only when !saw_space ->
      (Printf.eprintf "Error: line %d uses spaces for indentation; you asked for -tabonly\n" line_num;
       exit 2)
    | Space_only when !saw_tab ->
      (Printf.eprintf "Error: line %d uses tabs for indentation; you asked for -spaceonly\n" line_num;
       exit 2)
    | Either when !saw_space && !saw_tab ->
      Printf.eprintf "Warning: line %d uses mixed space and tab indentation.\n" line_num
    | _ -> ()

(*
  parse pass 1: read in the source code and transform it into a sequence of meaningful lines by:
   - merge blank lines into the next meaningful line
   - merge comment lines into the next meaningful line
   - merge dangling curly-braced, square-bracketed, or parenthesized lines into
     the previous meaningful line
   - determine the type and indent-level of each meaningful line
*)

type syntax_pass1 = meaningful_line list
and meaningful_line = line_type * int * int * string (* line_type, indent_count, line_number, line_text *)

let parse_pass1 reader =
  let rec dangling_lines () =
    match try Some (reader#peek ()) with End_of_file -> None with
      Some line ->
      let lexstate = reader#lexical_state () in
      if lexstate.quote || lexstate.comment > 0 || lexstate.square > 0 || lexstate.curly > 0 || lexstate.paren > 0 then
        begin
          reader#drop ();
          endl ^ line ^ (dangling_lines ())
        end
      else
        ""
    | None -> ""
  in
  let next_meaningful_line () =
    let rec iter meaningless_lines =
      let line = reader#read () in
      let lexstate = reader#lexical_state () in
      if lexstate.comment = 0 && not (is_blank line) then
        let ty = try determine_line_type line with Invalid_argument _ -> Printf.eprintf "syntax error at line %d\n" (reader#line_number ()); exit 2 in
        let ln = reader#line_number () in
        let dangle = dangling_lines () in
        check_indentation ln line;
        (ty,indent_count line,ln,meaningless_lines ^ line ^ dangle)
      else 
        iter (meaningless_lines ^ line ^ endl)
    in
    iter ""
  in
  let lines = ref [] in
  begin
    try
      while true do 
        lines := (next_meaningful_line ()) :: !lines
      done
    with
      End_of_file -> ()
  end;
  List.rev !lines


(* here's our extremely simple abstract syntax tree *)
type syntax = syntactic_unit list
and syntactic_unit =
    Line of line_type*int*string    (* line_type,line_number,line_text *)
  | Block of syntax
  | PipeBlock of syntax

(* parse pass 2: collect lines at the same indent-level into blocks and sub-blocks *)

let parse_pass2 lines =
  let stream = Stream.of_list lines in
  let rec level n =
    match Stream.peek stream with
      Some (ty,n',line_num,txt) when n = n' -> Stream.junk stream; (Line (ty,line_num,txt)) :: (level n)
    | Some (ty,n',line_num,txt) when n < n' ->
      let sublevel = level n' in
      (Block sublevel) :: (level n)
    | _ -> []
  in
  level 0

(* parse pass 3: (postprocessing) change Blocks with only pipe lines or sub-blocks (i.e. patterns) into PipeBlocks *)

let rec collect_pipe_blocks = function
    (Block syntax) :: rest ->
    let any_pipes = List.exists (function (Line (Pipe,_,_)) -> true | _ -> false) syntax in
    let all_pipes = not (List.exists (function (Line (Pipe,_,_)) | (Block _) | (PipeBlock _) -> false | _ -> true) syntax) in
    if any_pipes && all_pipes then
      (PipeBlock (collect_pipe_blocks syntax)) :: (collect_pipe_blocks rest)
    else
      (Block (collect_pipe_blocks syntax)) :: (collect_pipe_blocks rest)
  | fst :: rest -> fst :: (collect_pipe_blocks rest)
  | [] -> []

(* aight *)
let parse reader = 
  let ml = parse_pass1 reader in
  let syntax = parse_pass2 ml in
  let postprocessed = collect_pipe_blocks syntax in
  postprocessed

(******************************************************************************
   syntax tree pretty-printing (mosty for debugging)
 ******************************************************************************)

let string_of_ty = function
    Identifier -> "ID "
  | Curly -> "Crl"
  | Square -> "Sqr"
  | Quote -> "Qut"
  | Let -> "Let"
  | In -> "In "
  | And -> "And"
  | If -> "If "
  | Else -> "Els"
  | For -> "For"
  | While -> "Whl"
  | Paren -> "Par"
  | Pipe -> "Pip"
  | Function -> "Fnc"
  | Match -> "Mch"
  | Try -> "Try"
  | With -> "Wth"
  | Open -> "Opn"
  | Fun -> "Fun"
  | Type -> "Typ"
  | Module -> "Mod"
  | ModuleType -> "MTy"
  | Struct -> "Str"
  | Sig -> "Sig"
  | Val -> "Val"
  | Class -> "Cls"
  | Method -> "Mth"
  | Object -> "Obj"
  | Initializer -> "Ini"
  | Inherit -> "Inh"
  | NamedOperand -> "Nmd"
  | OptionalOperand -> "Opt"
  | Constraint -> "Cns"
  | Exception -> "Exn"
  | Include -> "Inc"
;;
let rec print_block_syntax pfx level syntax =
  List.iter
    (function
        Line (ty,_,line) -> Printf.eprintf "%c%d%s %s\n" pfx level (string_of_ty ty) line
      | Block block -> print_block_syntax 'B' (level + 1) block
      | PipeBlock block -> print_block_syntax 'P' (level + 1) block)
    syntax;;

(******************************************************************************
   OCaml syntax formation
 ******************************************************************************)

let rec nearest_line_number = function
    (Line (_,num,_)) :: rest -> num
  | (Block block) :: rest -> nearest_line_number block
  | (PipeBlock block) :: rest -> nearest_line_number block
  | [] -> 9999999

let rec form_expression form_rest = function

    (Line (Let,_,letline)) :: (((Line (_,_,_)) :: _) as rest) ->
    endl ^ letline ^ (form_ands (form_in form_rest) rest)
  | (Line (Let,_,letline)) :: (Block block) :: rest ->
    endl ^ letline ^ " (" ^ (form_sequence block) ^ " )" ^ (form_ands (form_in form_rest) rest)
  | (Line (Let,_,letline)) :: (PipeBlock block) :: rest ->
    endl ^ letline ^ (form_patterns block) ^ (form_ands (form_in form_rest) rest)

(*
    (* it would be preferable to use begin and end instead of parentheses in the first and fourth let clauses, but this breaks object constructors (class c = let name = value in object ... end) due to ocamlc bug^H^H^Hirregularities *)
    (Line (Let,_,letline)) :: (Block block) :: rest ->
      endl ^ letline ^ " (" ^ (form_sequence block) ^ " )" ^ (form_ands (form_ins form_rest) rest)
  | (Line (Let,_,letline)) :: (PipeBlock block) :: rest ->
      endl ^ letline ^ (form_patterns block) ^ (form_ands (form_ins form_rest) rest)
  | (Line (Let,_,letline)) :: ((Line (In,_,_) :: _) as rest) ->
      endl ^ letline ^ (form_ands (form_ins form_rest) rest)
  | (Line (Let,_,letline)) :: ((Line (Let,_,_) :: _) as rest) ->
      endl ^ letline ^ " (" ^ (form_sequence rest) ^ " )" ^ (form_rest [])
*)
  | (Line (If,_,ifline)) :: (Block block) :: rest ->
    endl ^ ifline ^ " begin" ^ (form_sequence block) ^ " end" ^ (form_elses form_rest rest)
  | (Line (If,_,ifline)) :: rest ->
    endl ^ ifline ^ (form_elses form_rest rest)

  | (Line (loopty,_,loopline)) :: (Block block) :: rest when loopty = For || loopty = While ->
    endl ^ loopline ^ (form_sequence block) ^ " done" ^ (form_rest rest)
  | (Line (loopty,_,loopline)) :: rest when loopty = For || loopty = While ->
    endl ^ loopline ^ " done" ^ (form_rest rest)

  | (Line (Fun,_,line)) :: (Block block) :: rest -> " (" ^ endl ^ line ^ (form_sequence block) ^ " )" ^ (form_rest rest)

  | (Line (Function,_,line)) :: (PipeBlock block) :: rest | (Line (Match,_,line)) :: (PipeBlock block) :: rest ->
    endl ^ line ^ (form_patterns block) ^ (form_rest rest)
  | (Line (Match,_,matchline)) :: (Block block) :: (Line (With,_,withline)) :: rest ->
    endl ^ matchline ^ " begin" ^ (form_sequence block) ^ " end" ^ endl ^ withline ^
      (match rest with
         PipeBlock patterns :: rest ->
         (form_patterns patterns) ^ (form_rest rest)
       | _ -> form_rest rest)

  | (Line (Try,_,tryline)) :: (Block block) :: (Line (With,_,withline)) :: rest ->
    endl ^ tryline ^ " begin" ^ (form_sequence block) ^ " end" ^ endl ^ withline ^
      (match rest with
         PipeBlock patterns :: rest ->
         (form_patterns patterns) ^ (form_rest rest)
       | Block block :: rest -> " begin" ^ (form_sequence block) ^ " end" ^ (form_rest rest)
       | _ -> form_rest rest)

  | (Line (Try,_,tryline)) :: (Line (With,_,withline)) :: rest ->
    endl ^ tryline ^ endl ^ withline ^
      (match rest with
       | PipeBlock patterns :: rest -> (form_patterns patterns) ^ (form_rest rest)
       | Block block :: rest -> " begin" ^ (form_sequence block) ^ " end" ^ (form_rest rest)
       | _ -> form_rest rest)

  (* immediate objects *)
  | (Line (Object,_,line)) :: (Block block) :: rest ->
    endl ^ line ^ (form_object_contents block) ^ " end" ^ (form_rest rest)
  | (Line (Object,_,line)) :: rest ->
    endl ^ line ^ " end" ^ (form_rest rest)

  (* local modules *)
  | (Line (Struct,_,structline)) :: (Block block) :: rest ->
    endl ^ structline ^ (form_module_sequence block) ^ " end" ^ (form_rest rest)
  | (Line (Struct,_,structline)) :: rest ->
    endl ^ structline ^ " end" ^ (form_rest rest)
  | (Line (Sig,_,sigline)) :: (Block block) :: rest ->
    endl ^ sigline ^ (form_module_type_contents block) ^ " end" ^ (form_rest rest)
  | (Line (Sig,_,sigline)) :: rest ->
    endl ^ sigline ^ " end" ^ (form_rest rest)

  | (Line (_,_,line)) :: (PipeBlock block) :: rest -> endl ^ line ^ (form_patterns block) ^ (form_rest rest)
  | (Line (Identifier,_,line)) :: (Block block) :: rest -> endl ^ line ^ (form_application_operands block) ^ (form_rest rest)
  | (Line (Paren,_,line)) :: (Block block) :: rest -> endl ^ line ^ (form_application_operands block) ^ (form_rest rest)
  | (Line (_,_,line)) :: (Block block) :: rest -> " (" ^ endl ^ line ^ " )" ^ (form_application_operands block) ^ (form_rest rest)
  | (Line (_,_,line)) :: rest  -> endl ^ line ^ (form_rest rest)
  | (Block block) :: rest -> failwith (Printf.sprintf "unexpected block at line %d" (nearest_line_number block))
  | (PipeBlock block) :: rest -> failwith (Printf.sprintf "unexpected pipeblock at line %d" (nearest_line_number block))
  | [] -> ""

and form_ands form_rest = function
    (Line (And,_,andline)) :: (Block block) :: rest ->
    endl ^ andline ^ " begin" ^ (form_sequence block) ^ " end" ^ (form_ands form_rest rest)
  | (Line (And,_,andline)) :: (PipeBlock block) :: rest ->
    endl ^ andline ^ (form_patterns block) ^ (form_ands form_rest rest)
  | (Line (And,_,andline)) :: rest ->
    endl ^ andline ^ (form_ands form_rest rest)
  | rest -> (form_rest rest)
and form_in form_rest rest =
  " in (" ^ (form_sequence rest) ^ ")" ^ (form_rest [])

(*    (Line (In,_,inline)) :: (Block block) :: rest ->
      endl ^ inline ^ " begin" ^ (form_sequence block) ^ " end" ^ (form_ins form_rest rest)
      | (Line (In,_,inline)) :: ((Line (Let,_,_) :: _) as rest) ->
      endl ^ inline ^ " begin" ^ (form_sequence rest) ^ " end" ^ (form_rest [])
      | (Line (In,_,inline)) :: rest ->
      endl ^ inline ^ (form_ins form_rest rest)
      | rest -> form_rest rest *)

and form_elses form_rest = function
    (Line (Else,_,elseline)) :: (Block block) :: rest ->
    endl ^ elseline ^ " begin" ^ (form_sequence block) ^ " end" ^ (form_elses form_rest rest)
  | (Line (Else,_,elseline)) :: rest ->
    endl ^ elseline ^ (form_elses form_rest rest)
  | rest -> form_rest rest

and form_naked_expressions syntax =
  form_expression form_naked_expressions syntax

and form_sequence = function
    [] -> ""
  | syntax -> " (" ^ (form_expression (fun rest -> " )" ^ form_rest_sequence rest) syntax)
and form_rest_sequence = function
    [] -> ""
  | syntax -> "; (" ^ (form_expression (fun rest -> " )" ^ form_rest_sequence rest) syntax)

and form_application_operands = function
    [] -> ""
  | (Line (NamedOperand,_,line)) :: rest -> endl ^ line ^ (form_application_operands rest)
  | (Line (OptionalOperand,_,line)) :: rest -> endl ^ line ^ (form_application_operands rest)
  | syntax -> " (" ^ (form_expression (fun rest -> " )" ^ form_application_operands rest) syntax)

and form_patterns = function
    (Line (Pipe,_,pipeline)) :: (Block block) :: rest ->
    endl ^ pipeline ^ " begin" ^ (form_sequence block) ^ " end" ^ (form_patterns rest)
  | (Line (Pipe,_,pipeline)) :: rest ->
    endl ^ pipeline ^ (form_patterns rest)
  | [] -> ""
  | (Line (_,num,_)) :: rest -> failwith (Printf.sprintf "unexpected in pattern block at line %d; this shouldn't happen" num)
  | ((Block block) :: rest) as x -> failwith (Printf.sprintf "unexpected block at line %d" (nearest_line_number x))
  | ((PipeBlock block) :: rest) as x -> failwith (Printf.sprintf "unexpected pipeblock at line %d" (nearest_line_number x))

and form_object_contents = function
    [] -> ""
  | (Line (Val,_,line)) :: (Block block) :: rest ->
    endl ^ line ^ " begin" ^ (form_sequence block) ^ " end" ^ (form_object_contents rest)
  | (Line (Val,_,line)) :: (PipeBlock block) :: rest ->
    endl ^ line ^ (form_patterns block) ^ (form_object_contents rest)
  | (Line (Val,_,line)) :: rest ->
    endl ^ line ^ (form_object_contents rest)

  | (Line (Method,_,line)) :: (Block block) :: rest ->
    endl ^ line ^ " begin" ^ (form_sequence block) ^ " end" ^ (form_object_contents rest)
  | (Line (Method,_,line)) :: (PipeBlock block) :: rest ->
    endl ^ line ^ (form_patterns block) ^ (form_object_contents rest)
  | (Line (Method,_,line)) :: rest ->
    endl ^ line ^ (form_object_contents rest)

  | (Line (Initializer,_,line)) :: (Block block) :: rest ->
    endl ^ line ^ " begin" ^ (form_sequence block) ^ " end" ^ (form_object_contents rest)
  | (Line (Initializer,_,line)) :: (PipeBlock block) :: rest ->
    endl ^ line ^ (form_patterns block) ^ (form_object_contents rest)
  | (Line (Initializer,_,line)) :: rest ->
    endl ^ line ^ (form_object_contents rest)

  | (Line (Inherit,_,line)) :: rest ->
    endl ^ line ^ (form_object_contents rest)

  | (Line (Constraint,_,line)) :: rest ->
    endl ^ line ^ (form_object_contents rest)     

  | _ as lst -> failwith (Printf.sprintf "unexpected in object body at line %d" (nearest_line_number lst))

(* for recursive object types *)
and form_object_ands form_rest = function

  | (Line (And,_,andline)) :: (Block ((Line (Let,_,_) :: _) as block)) :: rest
  | (Line (And,_,andline)) :: (Block ((Line (Object,_,_) :: _) as block)) :: rest ->
    endl ^ andline ^ (form_sequence block) ^ (form_object_ands form_rest rest)
  | (Line (And,_,andline)) :: (Block block) :: rest ->
    endl ^ andline ^ (form_object_contents block) ^ " end" ^ (form_object_ands form_rest rest)
  | (Line (And,_,andline)) :: (Line (Object,_,structline)) :: (Block block) :: rest ->
    endl ^ andline ^ endl ^ structline ^ (form_object_contents block) ^ " end" ^ (form_object_ands form_rest rest)
  | (Line (And,_,line)) :: (Line (Object,_,structline)) :: rest ->
    endl ^ line ^ endl ^ structline ^ " end" ^ (form_object_ands form_rest rest)
  | (Line (And,_,andline)) :: rest ->
    endl ^ andline ^ " end" ^ (form_object_ands form_rest rest)
  | rest -> (form_rest rest)


and form_module_type_contents = function
    [] -> ""
  | (Line (Type,_,typeline)) :: (PipeBlock block) :: rest ->
    endl ^ typeline ^ (form_patterns block) ^ (form_ands form_module_type_contents rest)
  | (Line (Type,_,typeline)) :: rest ->
    endl ^ typeline ^ (form_ands form_module_type_contents rest)
  | (Line (Open,_,line)) :: rest ->
    endl ^ line ^ (form_module_type_contents rest)
  | (Line (Exception,_,line)) :: rest ->
    endl ^ line ^ (form_module_type_contents rest)
  | (Line (Val,_,line)) :: rest ->
    endl ^ line ^ (form_module_type_contents rest)
  | (Line (Include,_,line)) :: rest ->
    endl ^ line ^ (form_module_type_contents rest)

  | (Line (Module,_,line)) :: (Block ((Line (Sig,_,_) :: _) as block)) :: rest
  | (Line (ModuleType,_,line)) :: (Block ((Line (Sig,_,_) :: _) as block)) :: rest ->
    endl ^ line ^ (form_sequence block) ^ (form_module_type_contents rest)

  | (Line (Module,_,line)) :: (Block block) :: rest
  | (Line (ModuleType,_,line)) :: (Block block) :: rest ->
    endl ^ line ^ (form_module_type_contents block) ^ " end" ^ (form_module_type_contents rest)
  | (Line (Module,_,line)) :: (Line (Sig,_,sigline)) :: (Block block) :: rest
  | (Line (ModuleType,_,line)) :: (Line (Sig,_,sigline)) :: (Block block) :: rest ->
    endl ^ line ^ endl ^ sigline ^ (form_module_type_contents block) ^ " end" ^ (form_module_type_contents rest)

  | (Line (Module,_,line)) :: rest
  | (Line (ModuleType,_,line)) :: rest ->
    endl ^ line (* ^ " end" *) ^ (form_module_type_contents rest) (* had to disable the end to allow: module Make (Q : IntervalType) : S with type t = Q.t *)

  | (Line (Class,_,line)) :: (Block ((Line (Object,_,_) :: _) as block)) :: rest ->
    endl ^ line ^ (form_naked_expressions block) ^ " " ^ (form_object_ands form_module_type_contents rest) (* the form_naked_expressions is a hack because (object ... end) with parentheses is inexplicably a syntax error *)
  | (Line (Class,_,line)) :: (Block block) :: rest ->
    endl ^ line ^ (form_object_contents block) ^ " end" ^ (form_object_ands form_module_type_contents rest)
  | (Line (Class,_,line)) :: (Line (Object,_,structline)) :: (Block block) :: rest ->
    endl ^ line ^ endl ^ structline ^ (form_object_contents block) ^ " end" ^ (form_object_ands form_module_type_contents rest)
  | (Line (Class,_,line)) :: (Line (Object,_,structline)) :: rest ->
    endl ^ line ^ endl ^ structline ^ " end" ^ (form_object_ands form_module_type_contents rest)
  | (Line (Class,_,line)) :: rest ->
    endl ^ line ^ " end" ^ (form_object_ands form_module_type_contents rest)


  | _ as lst -> failwith (Printf.sprintf "unexpected in module type at line %d" (nearest_line_number lst))

and form_module_contents form_rest = function
    [] -> ""

  | (Line (Type,_,typeline)) :: (PipeBlock block) :: rest ->
    endl ^ typeline ^ (form_patterns block) ^ (form_ands form_rest rest)
  | (Line (Type,_,typeline)) :: rest ->
    endl ^ typeline ^ (form_ands form_rest rest)

  | (Line (Open,_,line)) :: rest ->
    endl ^ line ^ (form_rest rest)
  | (Line (Include,_,line)) :: rest ->
    endl ^ line ^ (form_rest rest)

  | (Line (Exception,_,line)) :: rest ->
    endl ^ line ^ (form_rest rest)

  | (Line (ModuleType,_,line)) :: (Block ((Line (Sig,_,_) :: _) as block)) :: rest ->
    endl ^ line ^ (form_sequence block) ^ (form_rest rest)
  | (Line (ModuleType,_,line)) :: (Block block) :: rest ->
    endl ^ line ^ (form_module_type_contents block) ^ " end" ^ (form_rest rest)
  | (Line (ModuleType,_,line)) :: (Line (Sig,_,sigline)) :: (Block block) :: rest ->
    endl ^ line ^ endl ^ sigline ^ (form_module_type_contents block) ^ " end" ^ (form_rest rest)
  | (Line (ModuleType,_,line)) :: rest ->
    endl ^ line ^ " end" ^ (form_rest rest)

  | (Line (Module,_,line)) :: (Block ((Line (Struct,_,_) :: _) as block)) :: rest ->
    endl ^ line ^ (form_sequence block) ^ (form_rest rest)
  | (Line (Module,_,line)) :: (Block block) :: rest ->
    endl ^ line ^ (form_module_sequence block) ^ " end" ^ (form_rest rest)
  | (Line (Module,_,line)) :: (Line (Struct,_,structline)) :: (Block block) :: rest ->
    endl ^ line ^ endl ^ structline ^ (form_module_sequence block) ^ " end" ^ (form_rest rest)

  (* to allow module N = MyFunctor(M) *)
  | (Line (Module,_,line)) :: rest ->
    endl ^ line ^ (form_rest rest)

  (*
    class c =
     let name = value in
      object
       ...
  *)
  | (Line (Class,_,line)) :: (Block ((Line (Let,_,_) :: _) as block)) :: rest
  (*
    class c =
     object
      ...
  *)
  | (Line (Class,_,line)) :: (Block ((Line (Object,_,_) :: _) as block)) :: rest ->
    endl ^ line ^ (form_naked_expressions block) ^ " " ^ (form_object_ands form_rest rest)
  (*
    class c = object
     ...
  *)
  | (Line (Class,_,line)) :: (Block block) :: rest ->
    endl ^ line ^ (form_object_contents block) ^ " end" ^ (form_object_ands form_rest rest)
   (*
    class c =
    object
     ...
  *)
  | (Line (Class,_,line)) :: (Line (Object,_,structline)) :: (Block block) :: rest ->
    endl ^ line ^ endl ^ structline ^ (form_object_contents block) ^ " end" ^ (form_object_ands form_rest rest)
  (* probably an unnecessary clause:
     class c =
     object ...
  *)
  | (Line (Class,_,line)) :: (Line (Object,_,structline)) :: rest ->
    endl ^ line ^ endl ^ structline ^ " end" ^ (form_object_ands form_rest rest)
  (* class c = object ... *)
  | (Line (Class,_,line)) :: rest ->
    endl ^ line ^ " end" ^ (form_object_ands form_rest rest)

  (* special case of toplevel lets, they don't need in. *)
  | (Line (Let,_,letline)) :: (((Line (_,_,_)) :: _) as rest) ->
    endl ^ letline ^ (form_ands form_rest rest)
  | (Line (Let,_,letline)) :: (Block block) :: rest ->
    endl ^ letline ^ " (" ^ (form_sequence block) ^ " )" ^ (form_ands form_rest rest)
  | (Line (Let,_,letline)) :: (PipeBlock block) :: rest ->
    endl ^ letline ^ (form_patterns block) ^ (form_ands form_rest rest)


  | syntax -> (form_expression form_rest syntax)

and form_module_sequence = function
    [] -> ""
  | syntax -> (form_module_contents form_rest_module_sequence syntax)
and form_rest_module_sequence = function
    [] -> ""
  | syntax -> " ;;" ^ (form_module_contents form_rest_module_sequence syntax)


(******************************************************************************
   main
 ******************************************************************************)

;;
type srctype =
    ML
  | MLI

let ty = ref None
let showblocks = ref false
let out_fn = ref ""

let arg_spec =
  Arg.align
    [
      ("-o",Arg.Set_string(out_fn),"<filename> output to specified file instead of standard out");
      ("-spaceonly",Arg.Unit (fun () -> config.whitespace_mode <- Space_only)," only allow spaces for indentation (default either spaces or tabs allowed and counted equally)");
      ("-tabonly",Arg.Unit (fun () -> config.whitespace_mode <- Tab_only)," only allow tabs for indentation");
      ("-ml",Arg.Unit (fun () -> ty := Some ML)," consider the input an implementation (.ml) file, regardless of its extension");
      ("-mli",Arg.Unit (fun () -> ty := Some MLI)," consider the input an interface (.mli) file, regardless of its extension");
      ("-lwt",Arg.Unit (fun () -> config.lwt <- true)," support Lwt syntax extension keywords: lwt, {try,for,raise}_lwt");
      ("-showblocks",Arg.Set(showblocks)," (for debugging) print the source code's block structure to standard error")
    ];;

let usage_msg =
  "Usage: ocaml+twt [options] source.ml\n" ^
    " normally the preprocessor should be invoked through ocamlc, e.g.\n" ^
    "  ocamlc -pp ocaml+twt source.ml\n" ^
    " to invoke the preprocessor with options through ocamlc, quote the command, e.g.\n" ^
    "  ocamlc -pp \"ocaml+twt -spaceonly\" source.ml\n" ^
    " alternatively, the input may be piped, e.g.\n" ^
    "  cat source.ml | ocaml+twt | ocaml\n" ^
    " options:"

let input_fname = ref "";;
Arg.parse arg_spec (fun s -> input_fname := s) usage_msg;;
let input_fname = !input_fname;;
let showblocks = !showblocks;;
let out_fn = !out_fn
let stdin_tty = Unix.isatty Unix.stdin;;

if stdin_tty && input_fname = "" then
  (Arg.usage arg_spec usage_msg;
   exit 2);;

let ty =
  match !ty with
    Some x -> x
  | None ->
    if input_fname = "" || Filename.check_suffix input_fname ".ml" || Filename.check_suffix input_fname ".ml+twt" then
      ML
    else if Filename.check_suffix input_fname ".mli" || Filename.check_suffix input_fname ".mli+twt" then
      MLI
    else
      (Printf.eprintf "ocaml+twt: I don't know what to do with %s; tell me -ml or -mli\n" input_fname;
       exit 2);;

let chan = if input_fname <> "" then open_in input_fname else stdin
let reader = new line_reader chan;;
let syntax = parse reader;;

if showblocks then
  (print_block_syntax 'T' 0 syntax;
   flush_all ())

let printer =
  match ty with
    ML -> form_module_sequence
  | MLI -> form_module_type_contents
;;

let rslt = Str.replace_first (Str.regexp "\n") "" (printer syntax);;
let out_chan = if out_fn = "" then stdout else open_out out_fn;;
if input_fname <> "" then Printf.fprintf out_chan "#1 \"%s\"\n" input_fname;;
output_string out_chan rslt;;
if out_fn <> "" then close_out out_chan;;
