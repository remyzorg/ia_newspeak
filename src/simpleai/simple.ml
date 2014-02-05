(*
  C2Newspeak: compiles C code into Newspeak. Newspeak is a minimal language 
  well-suited for static analysis.
  Copyright (C) 2007-2011  Charles Hymans, Sarah Zennou
  
  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.
  
  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.
  
  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

  Charles Hymans
  EADS Innovation Works - SE/CS
  12, rue Pasteur - BP 76 - 92152 Suresnes Cedex - France
  email: charles.hymans@penjili.org

  Sarah Zennou
  email: sarah(dot)zennou(at)eads(dot)net
*)


module type T =
sig 
  type t = {
    globals: vid list;    (** program variables *)
    init: blk;            (** initialization block of globals *)
    fundecs: (fid, fundec) Hashtbl.t; (** table of all
					  declared functions *)
    src_lang: Newspeak.src_lang;  (** source programming language *)
  }
     
  and fundec = blk
      
  and blk = stmt list
      
  and stmt = stmtkind * Newspeak.location
      
  and stmtkind =
      Set of (lval * exp)                 (** assignment *)
    | If of (exp * blk * blk)             (** if then else *)
    | While of (exp * blk)                (** while loop *)
    | Call of funexp                      (** function call *)
    | Assert of assertion                 (** assertion *)

  and funexp = FunId of fid
    
  and lval = Global of vid                (** global variable *)
    
  and exp =
      Const of cst                        (** integer constant *)
    | Lval of lval                        (** left value *)
    | Random of (integer * integer)       (** random value *)
    | UnOp of (unop * exp)                (** unary operation *)
    | BinOp of (binop * exp * exp)        (** binary operation *)
	
  and cst = CInt of integer
    
  and unop = Not                          (** negation *)
      
  and binop = 
      PlusI                               (** addition *)
    | MinusI                              (** substraction *)
    | MultI                               (** multiplication *)
    | DivI                                (** division *)
    | Mod                                 (** modulo *)
    | Gt                                  (** strictly greater than *)
    | Eq                                  (** equality *)
	
  and bounds = integer * integer
      
  and assertion = (lval * cmp * cst)      (** x == c
					      x <= c *)
  and cmp =
      Equals
    | IsLess
	
  and vid = string
      
  and fid = string
      
  and integer = Int32.t
      
  val to_dot : t -> string -> unit

  val to_string: t -> string
    
  val string_of_unop: unop -> string
    
  val string_of_binop: binop -> string
    
  val string_of_loc: Newspeak.location -> string
    
  val string_of_lval: lval -> string
    
  val string_of_exp: exp -> string
    
  val string_of_stmtkind: stmtkind -> string
    
  val string_of_stmt: stmt -> string
    
  val string_of_blk: blk -> string

end
 
type t = {
  globals: vid list;
  init: blk;
  fundecs: (fid, fundec) Hashtbl.t;
  src_lang: Newspeak.src_lang;
}
    
and fundec = blk
    
and blk = stmt list
    
and stmt = stmtkind * Newspeak.location
    
and stmtkind =
    Set of (lval * exp)
  | If of (exp * blk * blk)
  | While of (exp * blk)
  | Call of funexp
  | Assert of assertion
      
and funexp = FunId of fid
  
and lval = Global of vid
  
and exp = 
    Const of cst
  | Lval of lval
  | Random of (integer * integer)
  | UnOp of (unop * exp)
  | BinOp of (binop * exp * exp)
      
and cst = CInt of integer
  
and unop = Not
    
and binop = PlusI | MinusI | MultI | DivI | Mod | Gt | Eq
    
and bounds = integer * integer
    
and assertion = (lval * cmp * cst)
    
and cmp = Equals | IsLess
    
and vid = string
    
and fid = string
    
and integer = Int32.t
    
let string_of_loc (_, line, _) = string_of_int line
  
let string_of_cst c =
  match c with
      CInt i -> Int32.to_string i
	
let string_of_unop op =
  match op with
      Not -> "!"
	
let string_of_binop op =
  match op with
      PlusI -> "+"
    | MinusI -> "-"
    | MultI -> "*"
    | DivI -> "/"
    | Mod -> "%"
    | Gt -> ">"
    | Eq -> "=="
	
let string_of_lval lv =
  match lv with
      Global v -> v
	
let string_of_funexp e =
  match e with
      FunId f -> f
	
let rec string_of_exp e =
  match e with
      Const c -> string_of_cst c
    | Lval lv -> string_of_lval lv
    | Random (l, u) -> "["^Int32.to_string l^", "^Int32.to_string u^"]"
    | UnOp (op, e) -> string_of_unop op^" "^string_of_exp e
    | BinOp (op, e1, e2) -> 
	"("^string_of_exp e1^" "^string_of_binop op^" "^string_of_exp e2^")"
	  
let string_of_cmp x =
  match x with
      Equals -> "=="
    | IsLess -> "<="
	
let string_of_assertion (lv, cmp, c) =
  string_of_lval lv^" "^string_of_cmp cmp^" "^string_of_cst c
    
let rec string_of_stmtkind margin x =
  match x with
    | Set (lv, e) -> string_of_lval lv^" = "^string_of_exp e^";"
    | If (e, br1, br2) -> 
	"if "^string_of_exp e^" {\n"
	^string_of_blk (margin^"  ") br1
	^"    "^margin^"} else {\n"
	^string_of_blk (margin^"  ") br2
	^"    "^margin^"}"
    | While (e, body) -> 
	"while "^string_of_exp e^" {\n"
	^string_of_blk (margin^"  ") body
	^"    "^margin^"}"
    | Call f -> string_of_funexp f^"();"
    | Assert x -> "assert "^string_of_assertion x^";"
	
and string_of_stmt margin (x, loc) =
  (string_of_loc loc)^": "^margin^string_of_stmtkind margin x
    
and string_of_blk margin x = 
  match x with
      x::tl -> string_of_stmt margin x^"\n"^string_of_blk margin tl
    | [] -> ""
	
let to_string prog =
  let res = ref "" in
  let string_of_global x = res := !res^"int "^x^";\n" in
  let string_of_fundec f body = 
    res := !res^"void "^f^"() {\n";
    res := !res^string_of_blk "  " body;
    res := !res^"}\n"
  in
    List.iter string_of_global (List.rev prog.globals);
    res := !res^string_of_blk "" prog.init;
    Hashtbl.iter string_of_fundec prog.fundecs;
    !res
      
let string_of_stmtkind = string_of_stmtkind ""
  
let string_of_stmt = string_of_stmt ""
  
let string_of_blk = string_of_blk ""

(* Generates a DOT based representation of the control flow graph of
   the program prog and writes it in the file names filename *)
type error = HasNoMain
exception Error of error

type state = St of stmt | End | RTE

type transitions =
  | TCons of transitions * transitions
  | Tr of string
  | EmptyTr

let rec string_of_transitions = function
  | TCons (s1, s2) -> (string_of_transitions s1) ^
      (string_of_transitions s2)
  | Tr t -> Format.sprintf "%s;\n" t
  | EmptyTr -> ""

let (++) d1 d2 = TCons (d1, d2)
  
open Format
let state f st =
  match st with End -> "End" | RTE -> "RTE" |
      St (stmt, loc) -> begin
        match stmt with
        | Set (lval, exp) ->
            sprintf "%s_%s: %s := %s" f (string_of_loc loc)
              (string_of_lval lval)
	      (string_of_exp exp)
        | If (exp, _, _) -> 
            sprintf "%s_%s: if %s" f (string_of_loc loc)
              (string_of_exp exp)
        | While (_, _) ->
            sprintf "%s_%s: while" f (string_of_loc loc)
        | Call (FunId funid) -> 
            sprintf "%s_%s: call %s" f (string_of_loc loc) funid
        | Assert _ ->     
            sprintf "%s_%s: assert" f (string_of_loc loc)
      end

let tr f s1 s2 = Tr (sprintf "\"%s\" -> \"%s\"" (state f s1) (state f s2))
let label t lb = match t with Tr s ->
  Tr (sprintf "%s [label=\"%s\"]" s lb) | EmptyTr | TCons _ -> t
  
let to_dot prog filename = 
  printf "%s@\n" (to_string prog);
  printf "======> !@\n";

  let rec tr_of_stmt f next ((s, _) as stmt) =
    let branch lb blk = begin match blk with
    | [] -> label (tr f (St stmt) next) lb
    | h::_ -> label (tr f (St stmt) (St h)) lb end ++ tr_of_stmts f next blk
    in
    match s with
    | Set _ -> tr f (St stmt) next
    | If (_, blk1, blk2) ->
        if blk1 = [] && blk2 = [] then tr f (St stmt) next
        else branch "then" blk1 ++ branch "else" blk2
    | While (exp, blk) ->
        tr f (St stmt) next ++ branch (string_of_exp exp) blk
    | Call (FunId called)->
        let called_body = try Hashtbl.find prog.fundecs called with
          Not_found -> assert false in
        begin match called_body with
        | [] -> tr f (St stmt) next
        | first::_ -> tr f (St stmt) (St first)
        end
    | Assert assertion ->
        tr f (St stmt) RTE ++
          label (tr f (St stmt) next) (string_of_assertion assertion)

  and tr_of_stmts f next stmts = match stmts with
  | [] -> EmptyTr
  | [stmt] -> tr_of_stmt f next stmt
  | stmt1::(stmt2::_ as tail) -> tr_of_stmt f (St stmt2) stmt1 ++
      tr_of_stmts f next tail 
  in
  let fun_to_dot f body = tr_of_stmts f End body in
  let main = try Hashtbl.find prog.fundecs "main" with Not_found ->
    raise (Error (HasNoMain)) in

  let fid = open_out filename in
  let fmt = formatter_of_out_channel fid in

  let str = sprintf "digraph %s {@\n%s}\n"
    Filename.(chop_extension (basename filename))
    (string_of_transitions (fun_to_dot "main" main))
  in

  fprintf fmt "%s" str;
  printf "%s" str;
  
  (* A compléter *)
  close_out fid













