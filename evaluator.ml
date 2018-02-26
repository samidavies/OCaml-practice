(*  This language introduces bindings. *)

(* Problem 1: Introduce a new datatype (after `exp` but before
   everything else) called `binding` to represent bindings. For now,
   have `binding` consist of only a single constructor, representing
   `val` bindings (like `val x = foo` in SML). The constructor should
   take a string representing a variable name to bind and an
   expression to bind it to.  *)

type expr =
    Constant of int 
    | Add of expr * expr
    | False
    | True
    | LogicalAnd of expr * expr 
    | IfThenElse of expr * expr * expr
    | LessThanOrEqualTo of expr * expr
    | Variable of string;;

type binding = (string * expr);;

type ty = Num | Bool;;

type value = 
    Integer of int
    | Boo of bool;;


exception WrongConstructor 
exception TypeCheck 
exception EmptyList

type dynamic_environment = (string * value) list;; 
type static_environment = (string * ty) list;;


let rec lookup env var =
    match env with
        | [] -> raise EmptyList
        | (str, valu)::xs -> 
            if (str == var) then valu else lookup xs var
;;

(* Problem 2: Write a function to evaluate bindings.

   Start by renaming your `eval` function to `eval_expr`.

   Then define a new function, `eval_binding` which takes the current
   dynamic environment and a binding, and returns a *new* dynamic
   environment. `eval_binding` should evaluate the binding in an
   analogous way to the way SML bindings are processed: first evaluate
   the expression in the current dynamic environment (using
   `eval_expr`), and then *extend* the environment with a new binding
   for the variable name to the resulting value. (You need to figure
   out exactly what "extend" means here.)
*)


let rec eval_expr dynenv e =
    let to_int (v: value): int = 
      match v with
        | Integer num -> num
        | _ -> raise WrongConstructor in
    let to_bool (v: value): bool =
      match v with
        | Boo b -> b
        | _ -> raise WrongConstructor in
    match e with
        | Constant c -> Integer c
        | Add (sum1, sum2) -> Integer (to_int(eval_expr dynenv sum1) + to_int(eval_expr dynenv sum2))
        | False -> Boo (false)
        | True -> Boo (true)
        | LogicalAnd (bool1, bool2) ->  Boo(to_bool (eval_expr dynenv bool1) && to_bool(eval_expr dynenv bool2))
        | LessThanOrEqualTo (expr1, expr2) -> 
            if to_bool(Boo (to_int(eval_expr dynenv expr1) <= to_int(eval_expr dynenv expr2)))
              then Boo true else Boo false
        | IfThenElse (cond, do1, do2) -> 
            if to_bool(eval_expr dynenv cond)
              then eval_expr dynenv do1 
              else eval_expr dynenv do2
        | Variable var -> lookup dynenv var
;;

let eval_binding env (st, exp) = 
    let valu = eval_expr env exp in 
    (st, valu)::env
;;


(* Problem 3: Write a function to typecheck bindings.

   Start by renaming `typecheck` to `typecheck_expr`.

   Then write a function `typecheck_binding` that takes the current
   static environment and a binding and returns a new static
   environment. `typecheck_binding` should typecheck the binding in an
   analogous way to how SML bindings are typechecked: first typecheck
   the expression in the current static environment, and then *extend*
   the static environment with a new binding for the variable to the
   resulting type. (You need to figure out exactly what "extend" means
   here.)
*)

let rec typecheck_expr env e : ty= 
    let int_check ex = 
        if (typecheck_expr env ex) == Num
        then ()
        else raise TypeCheck in
    let bool_check ex = 
        if (typecheck_expr env ex) == Bool
        then ()
        else raise TypeCheck in
    match e with
        | Constant c -> Num
        | Add (num1, num2) -> 
          (int_check num1; int_check num2; Num)
        | False -> Bool
        | True -> Bool
        | LogicalAnd (cond1, cond2) -> 
          (bool_check cond1; bool_check cond2; Bool)
        | IfThenElse (cond, do1, do2) ->
            let _ = bool_check cond in 
            let type1 = typecheck_expr env do1 in 
            let type2 = typecheck_expr env do2 in 
                if (type1 == type2)
                then type1
                else raise TypeCheck
        | LessThanOrEqualTo (expr1, expr2) ->
          (int_check expr1; int_check expr2; Bool)
        | Variable var -> lookup env var
;;

let typecheck_binding env (st, exp) = 
    let valu = typecheck_expr env exp in 
        (st, valu) :: env
;;

(* Problem 4: Write a few tests for evaluating and typechecking
   bindings. *)

(* Problem 5: Implement (the core of) a REPL for our expression
   language, by writing a function `repl` that takes a program
   (represented as a list of bindings) and processes it, analogous to
   how SML processes programs, starting from the empty dynamic/static
   environment, and returns a pair of the "final" dynamic/static
   environments.

   Be careful to typecheck things before evaluating them!

   Hint: define a helper function that takes the current
   dynamic/static environments as additional arguments.
*)

let repl (bind_list: binding list): (dynamic_environment * static_environment) =
    let statenv = List.fold_left typecheck_binding [] bind_list in
    let dynenv = List.fold_left eval_binding [] bind_list in 
    (dynenv, statenv)
;;


(* Problem 6: write a few test "programs" (lists of bindings) for
   your REPL, and bind them to SML variables of your choice here. *)


let test1 =  Add (Constant 3, Constant 4)
;;
let test2 = IfThenElse((LessThanOrEqualTo (Add (Constant 3,
                                                Constant 2), 
                                          Variable "seven"), True, (LogicalAnd (True, True))))
                                          ;;
let test3 = repl([("six", Constant 6); ("eight", Add(Constant 3, Constant 5)); ("true", True)]);;


let dynenv2 = [("seven", Integer 7)]