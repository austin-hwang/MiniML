(* 
                         CS 51 Final Project
                        MiniML -- Expressions
                             Spring 2018
*)

(*......................................................................
  Abstract syntax of MiniML expressions 
 *)

type unop =
  | Negate
  | Negatef
  | Not 
;;
    
type binop =
  | Plus
  | Minus
  | Times
  | Divide
  | Plusf
  | Minusf
  | Timesf
  | Dividef
  | Equals
  | NotEquals
  | LessThan
  | GreaterThan
  | LessEquals
  | GreaterEquals
  | And
  | Or 
  | Exponent
;;

type expr =
  | Var of varid                         (* variables *)
  | Num of int                           (* integers *)
  | Float of float
  | Bool of bool                         (* booleans *)
  | Unop of unop * expr                  (* unary operators *)
  | Binop of binop * expr * expr         (* binary operators *)
  | Conditional of expr * expr * expr    (* if then else *)
  | Fun of varid * expr                  (* function definitions *)
  | Let of varid * expr * expr           (* local naming *)
  | Letrec of varid * expr * expr        (* recursive local naming *)
  | Raise                                (* exceptions *)
  | Unassigned                           (* (temporarily) unassigned *)
  | App of expr * expr                   (* function applications *)
 and varid = string ;;
  
(*......................................................................
  Manipulation of variable names (varids)
 *)

(* varidset -- Sets of varids *)
module SS = Set.Make (struct
                       type t = varid
                       let compare = String.compare
                     end ) ;;

type varidset = SS.t ;;

(* same_vars :  varidset -> varidset -> bool
   Test to see if two sets of variables have the same elements (for
   testing purposes) *)
let same_vars : varidset -> varidset -> bool =
  SS.equal;;

(* vars_of_list : string list -> varidset
   Generate a set of variable names from a list of strings (for
   testing purposes) *)
let vars_of_list : string list -> varidset =
  SS.of_list ;;
  
(* free_vars : expr -> varidset
   Return a set of the variable names that are free in expression
   exp *)
let rec free_vars (exp : expr) : varidset =
    match exp with 
    | Var v -> SS.singleton v
    | Num _ | Float _ | Bool _ | Raise | Unassigned -> SS.empty
    | Unop (_, e) -> free_vars e 
    | Binop (_, e1, e2) -> SS.union (free_vars e1) (free_vars e2)
    | Conditional (e1, e2, e3) -> SS.union (free_vars e3) 
                                  (SS.union (free_vars e1) (free_vars e2))
    | Fun (v, e) -> SS.remove v (free_vars e)
    | Let (v, e1, e2) -> SS.union (SS.remove v (free_vars e2)) (free_vars e1)
    | Letrec (v, e1, e2) -> SS.remove v (SS.union (free_vars e2) (free_vars e1))
    | App (e1, e2) -> SS.union (free_vars e1) (free_vars e2) ;;

(* new_varname : unit -> varid
   Return a fresh variable, constructed with a running counter a la
   gensym. Assumes no variable names use the prefix "var". (Otherwise,
   they might accidentally be the same as a generated variable name.) *)

let new_varname : unit -> varid =
  let initial  = ref 0 in
  fun () ->
  let s = "x" ^ string_of_int !initial in 
  initial := !initial + 1; 
  s ;;

(*......................................................................
  Substitution 

  Substitution of expressions for free occurrences of variables is the
  cornerstone of the substitution model for functional programming
  semantics.
 *)

(* subst : varid -> expr -> expr -> expr
   Substitute repl for free occurrences of var_name in exp *)
let rec subst (var_name : varid) (repl : expr) (exp : expr) : expr =
  let rechelper (v : varid) (e1 : expr) (e2: expr) (isrec : bool) : expr =
    let substhelper (v : varid) (e1 : expr) (e2: expr) =
      if isrec then Letrec (v, e1, e2) else Let (v, e1, e2) 
    in
    if v = var_name then if isrec then exp 
    else Let (v, subst var_name repl e1, e2) 
    else if SS.mem v (free_vars repl) then let x = new_varname() in 
      substhelper x (subst var_name repl (subst v (Var x) e1)) 
                    (subst var_name repl e2)
    else substhelper v (subst var_name repl e1) (subst var_name repl e2)
  in
  match exp with 
  | Var v -> if v = var_name then repl else exp
  | Num _ | Float _ | Bool _ | Raise | Unassigned -> exp
  | Unop (u, e) -> Unop (u, subst var_name repl e)
  | Binop (u, e1, e2) -> 
      Binop (u, subst var_name repl e1, subst var_name repl e2) 
  | Conditional (e1, e2, e3) -> Conditional (subst var_name repl e1, 
                                             subst var_name repl e2, 
                                             subst var_name repl e3)
  | Fun (v, e) -> if v = var_name then exp
                  else if SS.mem v (free_vars repl) 
                  then let x = new_varname() in 
                    Fun (x, subst var_name repl (subst v (Var x) e)) 
                  else Fun (v, subst var_name repl e)
  | Let (v, e1, e2) -> rechelper v e1 e2 false
  | Letrec (v, e1, e2) -> rechelper v e1 e2 true
  | App (e1, e2) -> App (subst var_name repl e1, subst var_name repl e2) ;;

(*......................................................................
  String representations of expressions
 *)
let unophelper (u : unop) (isconcrete : bool) : string = 
  match u with
  | Negate -> if isconcrete then "~-" else "Negate" 
  | Negatef -> if isconcrete then "~-." else "Negatef" 
  | Not -> if isconcrete then "not" else "Not"  ;;

let binophelper (b : binop) (isconcrete : bool) : string = 
  match b with 
  | And -> if isconcrete then "&&" else "And"
  | Or -> if isconcrete then "||" else "Or"
  | Plus -> if isconcrete then "+" else "Plus"
  | Minus -> if isconcrete then "-" else "Minus"
  | Times -> if isconcrete then "*" else "Times"
  | Divide -> if isconcrete then "/" else "Divide"
  | Plusf -> if isconcrete then "+." else "Plusf"
  | Minusf -> if isconcrete then "-." else "Minusf"
  | Timesf -> if isconcrete then "*." else "Timesf"
  | Dividef -> if isconcrete then "/." else "Dividef"
  | Exponent -> if isconcrete then "**" else "Exponent"
  | Equals -> if isconcrete then "=" else "Equals"
  | NotEquals -> if isconcrete then "<>" else "NotEquals"
  | LessThan -> if isconcrete then "<" else "LessThan" 
  | GreaterThan -> if isconcrete then ">" else "GreaterThan" 
  | LessEquals -> if isconcrete then "<=" else "LessEquals" 
  | GreaterEquals -> if isconcrete then ">=" else "GreaterEquals" ;;


(* exp_to_concrete_string : expr -> string
   Returns a concrete syntax string representation of the expr *)
let rec exp_to_concrete_string (exp : expr) : string =
  match exp with 
  | Var v -> v
  | Num i ->  string_of_int i
  | Float f -> string_of_float f
  | Bool b -> string_of_bool b
  | Unop (u, e) -> unophelper u true ^ exp_to_concrete_string e
  | Binop (b, e1, e2) -> exp_to_concrete_string e1 ^ " " ^ binophelper b true ^
                         " " ^ exp_to_concrete_string e2
  | Conditional (e1, e2, e3) -> "if " ^ exp_to_concrete_string e1 ^ 
                                " then " ^ exp_to_concrete_string e2 ^ 
                                " else " ^ exp_to_concrete_string e3
  | Fun (v, e) -> "fun " ^ v ^ " -> " ^ exp_to_concrete_string e
  | Let (v, e1, e2) -> "let " ^ v ^ " = " ^ exp_to_concrete_string e1 ^ 
                       " in " ^ exp_to_concrete_string e2
  | Letrec (v, e1, e2) -> "let rec " ^ v ^ " = " ^ exp_to_concrete_string e1 ^ 
                          " in " ^ exp_to_concrete_string e2
  | App (e1, e2) -> exp_to_concrete_string e1 ^ " " ^ exp_to_concrete_string e2
  | Raise -> "raise" 
  | Unassigned -> "Unassigned" ;;

(* exp_to_abstract_string : expr -> string
   Returns a string representation of the abstract syntax of the expr *)
let rec exp_to_abstract_string (exp : expr) : string =
  match exp with 
  | Var v -> "Var(" ^ v ^ ")" 
  | Num i ->  "Num(" ^ string_of_int i ^ ")"
  | Float f -> "Float(" ^ string_of_float f ^ ")"
  | Bool b -> "Bool(" ^ string_of_bool b ^ ")"
  | Unop (u, e) -> "Unop(" ^ unophelper u true ^ 
                   ", " ^ exp_to_abstract_string e ^ ")"
  | Binop (b, e1, e2) -> "Binop(" ^ binophelper b false ^ 
    ", " ^ exp_to_abstract_string e1 ^ ", " ^ exp_to_abstract_string e2 ^ ")"
  | Conditional (e1, e2, e3) -> "Conditional(" ^ exp_to_abstract_string e1 ^ 
                                ", " ^ exp_to_abstract_string e2 ^ 
                                ", " ^ exp_to_abstract_string e3 ^ ")"
  | Fun (v, e) -> "Fun(" ^ v ^ ", " ^ exp_to_abstract_string e ^ ")"
  | Let (v, e1, e2) -> "Let(" ^ v ^ ", " ^ exp_to_abstract_string e1 ^ 
                       ", " ^ exp_to_abstract_string e2 ^ ")"
  | Letrec (v, e1, e2) -> "Letrec(" ^ v ^ ", " ^ exp_to_abstract_string e1 ^ 
                          ", " ^ exp_to_abstract_string e2 ^ ")"
  | App (e1, e2) -> "App(" ^ exp_to_abstract_string e1 ^ 
                    ", " ^ exp_to_abstract_string e2 ^ ")" 
  | Raise -> "Raise" 
  | Unassigned -> "Unassigned" ;;
