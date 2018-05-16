(*
	      A lightweight OCaml unit testing framework
                                 CS51

  A simple unit testing framework that also demonstrates use of a
  variety of programming techniques and paradigms from CS51.  
 *)
open Expr ;;
open Evaluation ;;
open Printf ;;
open Env ;;
(*--------------------------------------------------------------------
  Forcing a lazy computation in an environment that times out
  excessively long computations. *)

exception Timeout ;;

(* timeout time f -- Forces delayed computation f, returning what it
   returns, except that after time seconds it raises a Timeout
   exception. Based on the timeout function from
   https://caml.inria.fr/pub/docs/oreilly-book/html/book-ora168.html. *)

let sigalrm_handler = 
  Sys.Signal_handle (fun _ -> raise Timeout) ;;

let timeout (time : int) (f : 'a Lazy.t) : 'a =
  let old_behavior = 
    Sys.signal Sys.sigalrm sigalrm_handler in
  let reset_sigalrm () = 
    ignore (Unix.alarm 0); 
    Sys.set_signal Sys.sigalrm old_behavior in
  ignore (Unix.alarm time) ;
  let res = Lazy.force f in 
  reset_sigalrm () ; res ;;
  
(*--------------------------------------------------------------------
			The testing framework
 *)

type test =
    {label: string;          (* name of the test *)
     content: bool Lazy.t;   (* expression to execute for test; should
				return true on passing, false on failing 
				the test *)
     time: int;              (* amount of time to allow the test to run 
				before timing out, in seconds *)
     fail_msg: string        (* message to use if test fails *)
    } ;;

type status = 
  | Passed
  | Failed of string
  | Raised_exn of string
  | Timed_out of int ;;

(* test ?fail_msg ?time label content -- Returns a test with the given
   label and content, with optional fail_msg and timeout time that
   default appropriately. *)
let test ?(fail_msg="somehow") ?(time=5) label content =
  {label = label;
   content = content;
   fail_msg = fail_msg;
   time = time} ;;

(* run_test test continue -- Runs the test, applying the continue
   function to the test label and status. *)
let run_test ({label; time; content; fail_msg} : test) 
             (continue : string -> status -> unit) 
    : unit = 
  try
    if timeout time content
    then continue label Passed
    else continue label (Failed fail_msg)
  with
  | Timeout -> continue label (Timed_out time)
  | exn     -> continue label 
			(Raised_exn (Printexc.to_string exn)) ;;

(* present labels status -- Prints a line describing the out come of a
   test. Appropriate for use as the continue function in run_test. *)
let present label status =
  match status with
  | Passed -> printf "%s: passed\n" label
  | Failed msg -> 
      printf "%s: failed %s\n" label msg
  | Timed_out secs ->
      printf "%s: timed out in %d\n" label secs
  | Raised_exn msg ->
      printf "%s: raised %s\n" label msg ;;

(* report tests -- Generates a report based on the provided tests. *)
let report (tests: test list) : unit =
  List.iter (fun test -> run_test test present) tests ;;
  
(*......................................................................
  Sample tests to test the testing framework
 *)
  
let tests =
  [ test "should fail" (lazy (3 > 4)) ;
    test "should pass" (lazy (4 > 3)) ;
    test "should time out" (lazy (let rec f x = f x in f 1)) ;
    test "should raise exception" (lazy ((List.nth [0;1] 3) = 3))
  ] ;;
  
(* Run the tests if executing from the command line *)
let _ =
  try
    let _ = Str.search_forward
	      (Str.regexp "test_lite\\.\\(byte\\|native\\)")
	      (Sys.argv.(0))
	      0 in
    report tests
  with Not_found -> () ;;

(* A sample run:

    % ocamlbuild test_lite.byte
    Finished, 3 targets (2 cached) in 00:00:00.
    % ./test_lite.byte 
    should fail: failed somehow
    should pass: passed
    should time out: timed out in 5
    should raise exception: raised Failure("nth")
    %
 *)

(*1+2+3*)
let add = Binop(Plus, Binop(Plus, Num(1), Num(2)), Num(3)) ;;
(*1-2-3*)
let subtract = Binop(Minus, Binop(Minus, Num(1), Num(2)), Num(3)) ;;
(*1*2*3*)
let multiply = Binop(Times, Binop(Times, Num(1), Num(2)), Num(3)) ;;
(*10/5/3*)
let divide = Binop(Divide, Binop(Divide, Num(10), Num(5)), Num(3)) ;;
(*-10*)
let negate = Unop(Negate, Num(10)) ;;
(*1+2+3*)
let addf = Binop(Plusf, Binop(Plusf, Float(1.), Float(2.)), Float(3.)) ;;
(*1-2-3*)
let subtractf = Binop(Minusf, Binop(Minusf, Float(1.), Float(2.)), Float(3.)) ;;
(*1*2*3*)
let multiplyf = Binop(Timesf, Binop(Timesf, Float(1.), Float(2.)), Float(3.)) ;;
(*10/5/3*)
let dividef = Binop(Dividef, Binop(Dividef, Float(10.), Float(5.)), Float(4.)) ;;
(*-10*)
let negatef = Unop(Negatef, Float(10.)) ;;
(*not true*)
let not = Unop(Not, Bool(true)) ;;
(*1+2=2+1*)
let equals = Binop(Equals, Binop(Plus, Num(1), Num(2)), Binop(Plus, Num(2), Num(1))) ;;
(*1<>2*)
let notequals = Binop(NotEquals, Num(1), Num(2)) ;;
(*1<2*)
let lessthan = Binop(LessThan, Num(1), Num(2)) ;;
(*2>1*)
let greaterthan = Binop(GreaterThan, Num(2), Num(1)) ;;
(*1<=1*)
let lessequals = Binop(LessEquals, Num(1), Num(1)) ;;
(*1>=1*)
let greaterequals = Binop(GreaterEquals, Num(1), Num(1)) ;;
(*2**4*)
let exponent = Binop(Exponent, Float(2.), Float(4.)) ;;
(*(1<2) && (2>1)*)
let andtest = Binop(And, Binop(LessThan, Num(1), Num(2)), Binop(GreaterThan, Num(2), Num(1))) ;;
(*(1<2) || 1>2*)
let ortest = Binop(Or, Binop(LessThan, Num(1), Num(2)), Binop(GreaterThan, Num(1), Num(2))) ;;
(*(fun x -> x + x) 5*)
let funtest = App(Fun("x", Binop(Plus, Var("x"), Var("x"))), Num(5)) ;; (*10*)
(*let f = fun x -> x in f f 5*)
let testlet = Let("f", Fun("x", Var("x")), App(App(Var("f"), Var("f")), Num(10))) ;; (*10*)
(*let x = 1 in let f = fun y -> x + y in let x = 2 in f 1 ;;*)
let testlet2 = Let("x", Num(1), Let("f", Fun("y", Binop(Plus, Var("x"), Var("y"))), Let("x", Num(2), App(Var("f"), Num(1))))) (*2*)
(*let f = fun n -> if n = 0 then 1 else n * f (n-1) in f 2*)
let testlet3 = Let("f", Fun("n", Conditional(Binop(Equals, Var("n"), Num(0)), Num(1), Binop(Times, Var("n"), App(Var("f"), Binop(Minus, Var("n"), Num(1)))))), App(Var("f"), Num(2)))
(* let x = 1 in let f = fun y -> x + y in let x = 2 in f 3 ;;*)
let testlet4 = Let("x", Num(1), Let("f", Fun("y", Binop(Plus, Var("x"), Var("y"))), Let("x", Num(2), App(Var("f"), Num(3))))) ;;
(*let f = fun x -> if x = 10 then true else false in f 9*)
let conditional = Let("f", Fun("x", Conditional(Binop(Equals, Var("x"), Num(10)), Bool(true), Bool(false))), App(Var("f"), Num(9))) ;;
(*let x = 7 in let rec f = fun y -> if y = 0 then x else y * f (y - 1) in let x = 9 in f 3 ;;*)
let testletrec = Let("x", Num(7), Letrec("f", Fun("y", Conditional(Binop(Equals, Var("y"), Num(0)), Var("x"), Binop(Times, Var("y"), App(Var("f"), Binop(Minus, Var("y"), Num(1)))))), Let("x", Num(9), App(Var("f"), Num(3))))) ;;
(*let rec f = fun x -> if x = 0 then 1 else x * f (x - 1) in f 4 ;;*)
let testletrec2 = Letrec("f", Fun("x", Conditional(Binop(Equals, Var("x"), Num(0)), Num(1), Binop(Times, Var("x"), App(Var("f"), Binop(Minus, Var("x"), Num(1)))))), App(Var("f"), Num(4))) ;;

print_endline "Testing abstract expression \n" ;;

let testabstract = 
  [ test "Addition" (lazy ((exp_to_abstract_string add) = "Binop(Plus, Binop(Plus, Num(1), Num(2)), Num(3))")) ;
    test "Equals" (lazy ((exp_to_abstract_string equals) = "Binop(Equals, Binop(Plus, Num(1), Num(2)), Binop(Plus, Num(2), Num(1)))")) ;
    test "Fun" (lazy ((exp_to_abstract_string funtest) = "App(Fun(x, Binop(Plus, Var(x), Var(x))), Num(5))")) ;
    test "Let" (lazy ((exp_to_abstract_string testlet3) = "Let(f, Fun(n, Conditional(Binop(Equals, Var(n), Num(0)), Num(1), Binop(Times, Var(n), App(Var(f), Binop(Minus, Var(n), Num(1)))))), App(Var(f), Num(2)))")) ;
    test "Conditional" (lazy ((exp_to_abstract_string conditional) = "Let(f, Fun(x, Conditional(Binop(Equals, Var(x), Num(10)), Bool(true), Bool(false))), App(Var(f), Num(9)))")) ;
    test "Letrec" (lazy ((exp_to_abstract_string testletrec) = "Let(x, Num(7), Letrec(f, Fun(y, Conditional(Binop(Equals, Var(y), Num(0)), Var(x), Binop(Times, Var(y), App(Var(f), Binop(Minus, Var(y), Num(1)))))), Let(x, Num(9), App(Var(f), Num(3)))))")) ;
  ] ;;

report testabstract ;;
print_endline "Passed abstract string testing \n" ;;

print_endline "Testing free vars \n" ;;

let vartest = Var "x" ;;
let numtest = Num 1 ;;
let unoptest = Unop(Negate, Num 1) ;;
let binoptest = Binop(Plus, Num 1, Var "x") ;;
let freetest1 = Let("x", Num(3), Let("y", Var("x"), App(App(Var("f"), Var("x")), Var("y")))) ;;
let freetest2 = Let("x", Var("x"), Let("y", Var("x"), App(App(Var("f"), Var("x")), Var("y")))) ;;
let freetest3 = Let("x", Var("y"), Let("y", Var("x"), App(App(Var("f"), Var("x")), Var("y")))) ;;
let freetest4 = Let("x", Fun("y", Var("x")), Var("x")) ;;


let testfree = 
  [ test "Var" (lazy (same_vars(free_vars vartest) (vars_of_list ["x"]))) ;
    test "Num" (lazy (same_vars(free_vars numtest) (vars_of_list []))) ;
    test "Unop" (lazy (same_vars(free_vars unoptest) (vars_of_list []))) ;
    test "Binop" (lazy (same_vars(free_vars binoptest) (vars_of_list ["x"]))) ;
    test "Let Test 1" (lazy (same_vars(free_vars freetest1) (vars_of_list ["f"]))) ;
    test "Let Test 2" (lazy (same_vars(free_vars freetest2) (vars_of_list ["x"; "f"]))) ;
    test "Let Test 3" (lazy (same_vars(free_vars freetest3) (vars_of_list ["y"; "f"]))) ;
    test "Let Test 4" (lazy (same_vars(free_vars freetest4) (vars_of_list ["x"]))) ;
  ] ;;

report testfree ;;

print_endline "Passed free var testing\n" ;;

print_endline "Testing subst \n" ;;

let testsubst =
  [ test "Var" (lazy (subst "x" (Num 1) (Var "x") = Num 1)) ;
    test "Fun" (lazy (subst "y" (Num 1) (Fun("x", Binop(Plus, Var("x"), Var("y")))) = Fun ("x", Binop (Plus, Var "x", Num 1)))) ;
    test "Let" (lazy (subst "y" (Num 1) (Let("x", Num(5), Binop(Plus, Var("x"), Var("y")))) = (Let("x", Num(5), Binop(Plus, Var("x"), Num 1)))))
  ] ;;

report testsubst ;;
print_endline "Passed testing substitution \n" ;;

print_endline "Testing environment functions \n" ;;

let env = create () ;;
let env2 =  extend env "x" (ref (Val Unassigned)) ;;
let env3 = extend env2 "y" (ref (Val Unassigned)) ;;
let lookup1 = lookup env3 "y" ;;

let testenv = 
  [ test "Close" (lazy (close Unassigned env = Closure (Unassigned, env)));
    test "Extend" (lazy (env_to_string env2 = "(x, Unassigned), ")) ;
    test "Extend 2" (lazy (env_to_string env3 = "(y, Unassigned), (x, Unassigned), ")) ;
    test "Lookup" (lazy (value_to_string lookup1 = "Unassigned")) ;
  ] ;;

report testenv ;;
print_endline "Passed environment testing\n" ;;

print_endline "Testing eval substitution\n" ;;

let env = create () ;;

let testsubs =
  [ test "Addition" (lazy ((eval_s add env) = Val(Num(6)))) ;
    test "Subtraction" (lazy ((eval_s subtract env) = Val(Num(-4)))) ;
    test "Multiplication" (lazy ((eval_s multiply env) = Val(Num(6)))) ;
    test "Division" (lazy ((eval_s divide env) = Val(Num(0)))) ;
    test "Negation" (lazy ((eval_s negate env) = Val(Num(-10)))) ;
    test "Addition Float" (lazy ((eval_s addf env) = Val(Float(6.)))) ;
    test "Subtraction Float" (lazy ((eval_s subtractf env) = Val(Float(-4.)))) ;
    test "Multiplication Float" (lazy ((eval_s multiplyf env) = Val(Float(6.)))) ;
    test "Division Float" (lazy ((eval_s dividef env) = Val(Float(0.5)))) ;
    test "Negation Float" (lazy ((eval_s negatef env) = Val(Float(-10.)))) ;
    test "Not" (lazy ((eval_s not env) = Val(Bool(false)))) ;
    test "Equals" (lazy ((eval_s equals env) = Val(Bool(true)))) ;
    test "NotEquals" (lazy ((eval_s notequals env) = Val(Bool(true)))) ;
    test "LessThan" (lazy ((eval_s lessthan env) = Val(Bool(true)))) ;
    test "GreaterThan" (lazy ((eval_s greaterthan env) = Val(Bool(true)))) ;
    test "LessEquals" (lazy ((eval_s lessequals env) = Val(Bool(true)))) ;
    test "GreaterEquals" (lazy ((eval_s greaterequals env) = Val(Bool(true)))) ;
    test "Exponent" (lazy ((eval_s exponent env) = Val(Float(16.)))) ;
    test "And" (lazy ((eval_s andtest env) = Val(Bool(true)))) ;
    test "Or" (lazy ((eval_s ortest env) = Val(Bool(true)))) ;
    test "Conditional" (lazy ((eval_s conditional env) = Val(Bool(false)))) ;
    test "Fun" (lazy ((eval_s funtest env) = Val(Num(10)))) ;
    test "Let" (lazy ((eval_s testlet env) = Val(Num(10)))) ;
    test "Let 2" (lazy ((eval_s testlet2 env) = Val(Num(2)))) ;
    test "Let 3; Should raise EvalError" (lazy ((eval_s testlet3 env) = Val(Num(4)))) ;
    test "Let 4" (lazy ((eval_s testlet4 env) = Val(Num(4)))) ;
    test "Letrec" (lazy ((eval_s testletrec env) = Val(Num(42)))) ;
    test "Letrec 2" (lazy ((eval_s testletrec2 env) = Val(Num(24)))) ;
  ] ;;

report testsubs ;;

print_endline "Passed testing eval_s \n" ;;

print_endline "Testing dynamical expressions\n" ;;

let env = create () ;;

let testdynamical = 
 [  test "Addition" (lazy ((eval_d add env) = Val(Num(6)))) ;
    test "Subtraction" (lazy ((eval_d subtract env) = Val(Num(-4)))) ;
    test "Multiplication" (lazy ((eval_d multiply env) = Val(Num(6)))) ;
    test "Division" (lazy ((eval_d divide env) = Val(Num(0)))) ;
    test "Negation" (lazy ((eval_d negate env) = Val(Num(-10)))) ;
    test "Addition Float" (lazy ((eval_d addf env) = Val(Float(6.)))) ;
    test "Subtraction Float" (lazy ((eval_d subtractf env) = Val(Float(-4.)))) ;
    test "Multiplication Float" (lazy ((eval_d multiplyf env) = Val(Float(6.)))) ;
    test "Division Float" (lazy ((eval_d dividef env) = Val(Float(0.5)))) ;
    test "Negation Float" (lazy ((eval_d negatef env) = Val(Float(-10.)))) ;
    test "Not" (lazy ((eval_d not env) = Val(Bool(false)))) ;
    test "Equals" (lazy ((eval_d equals env) = Val(Bool(true)))) ;
    test "NotEquals" (lazy ((eval_d notequals env) = Val(Bool(true)))) ;
    test "LessThan" (lazy ((eval_d lessthan env) = Val(Bool(true)))) ;
    test "GreaterThan" (lazy ((eval_d greaterthan env) = Val(Bool(true)))) ;
    test "LessEquals" (lazy ((eval_d lessequals env) = Val(Bool(true)))) ;
    test "GreaterEquals" (lazy ((eval_d greaterequals env) = Val(Bool(true)))) ;
    test "Exponent" (lazy ((eval_d exponent env) = Val(Float(16.)))) ;
    test "And" (lazy ((eval_d andtest env) = Val(Bool(true)))) ;
    test "Or" (lazy ((eval_d ortest env) = Val(Bool(true)))) ;
    test "Conditional" (lazy ((eval_d conditional env) = Val(Bool(false)))) ; 
    test "Fun" (lazy ((eval_d funtest env) = Val(Num(10)))) ;
    test "Let" (lazy ((eval_d testlet env) = Val(Num(10)))) ;
    test "Let 2" (lazy ((eval_d testlet2 env) = Val(Num(3)))) ;
    test "Let 3" (lazy ((eval_d testlet3 env) = Val(Num(2)))) ;
    test "Let 4" (lazy ((eval_d testlet4 env) = Val(Num(5)))) ;
    test "Letrec" (lazy ((eval_d testletrec env) = Val(Num(54)))) ;
    test "Letrec 2" (lazy ((eval_d testletrec2 env) = Val(Num(24)))) ;
  ] ;;

report testdynamical ;;

print_endline "Passed dynamical testing\n" ;;

print_endline "Testing lexical expressions\n" ;;

let env = create () ;;

let testlexical = 
 [  test "Addition" (lazy ((eval_l add env) = Val(Num(6)))) ;
    test "Subtraction" (lazy ((eval_l subtract env) = Val(Num(-4)))) ;
    test "Multiplication" (lazy ((eval_l multiply env) = Val(Num(6)))) ;
    test "Division" (lazy ((eval_l divide env) = Val(Num(0)))) ;
    test "Negation" (lazy ((eval_l negate env) = Val(Num(-10)))) ;
    test "Addition Float" (lazy ((eval_l addf env) = Val(Float(6.)))) ;
    test "Subtraction Float" (lazy ((eval_l subtractf env) = Val(Float(-4.)))) ;
    test "Multiplication Float" (lazy ((eval_l multiplyf env) = Val(Float(6.)))) ;
    test "Division Float" (lazy ((eval_l dividef env) = Val(Float(0.5)))) ;
    test "Negation Float" (lazy ((eval_l negatef env) = Val(Float(-10.)))) ;
    test "Not" (lazy ((eval_l not env) = Val(Bool(false)))) ;
    test "Equals" (lazy ((eval_l equals env) = Val(Bool(true)))) ;
    test "NotEquals" (lazy ((eval_l notequals env) = Val(Bool(true)))) ;
    test "LessThan" (lazy ((eval_l lessthan env) = Val(Bool(true)))) ;
    test "GreaterThan" (lazy ((eval_l greaterthan env) = Val(Bool(true)))) ;
    test "LessEquals" (lazy ((eval_l lessequals env) = Val(Bool(true)))) ;
    test "GreaterEquals" (lazy ((eval_l greaterequals env) = Val(Bool(true)))) ;
    test "Exponent" (lazy ((eval_l exponent env) = Val(Float(16.)))) ;
    test "And" (lazy ((eval_l andtest env) = Val(Bool(true)))) ;
    test "Or" (lazy ((eval_l ortest env) = Val(Bool(true)))) ;
    test "Conditional" (lazy ((eval_l conditional env) = Val(Bool(false)))) ; 
    test "Fun" (lazy ((eval_l funtest env) = Val(Num(10)))) ;
    test "Let" (lazy ((eval_l testlet env) = Val(Num(10)))) ;
    test "Let 2" (lazy ((eval_l testlet2 env) = Val(Num(2)))) ;
    test "Let 3; Should raise EvalError" (lazy ((eval_l testlet3 env) = Val(Num(4)))) ;
    test "Let 4" (lazy ((eval_l testlet4 env) = Val(Num(4)))) ;
    test "Letrec" (lazy ((eval_l testletrec env) = Val(Num(42)))) ;
    test "Letrec 2" (lazy ((eval_l testletrec2 env) = Val(Num(24)))) ;
  ] ;;

report testlexical ;;

print_endline "Passed lexical testing\n" ;;
