(* non-tail-recursive solution *)
(* modified the solution to return boolean instead of int *)
(* https://stackoverflow.com/questions/31279920/finding-an-item-in-a-list-and-returning-its-index-ocaml *)
(* after I wrote to see if a value is in a list, I realize I could have just used List.mem *)
let rec find a b = 
	match b with 
	| [] -> false
	| head::rest -> if a = head then true else find a rest ;;


let rec subset a b = 
	match a with 
	| [] -> true
	| head :: rest -> if find head b then subset rest b else false ;;

(* if the b⊆a and a⊆b are both the same, then, by default, both lists are the same *)
let equal_sets a b = subset b a && subset a b ;;


let rec concat lst1 lst2 = 
  match lst1 with
  | [] -> lst2
  | h::t -> h::(concat t lst2) ;;

(* I used an existing concat function in ocaml, but then found out that the @ operater is the same thing *)
let set_union a b = a @ b ;;

(* simply set the if conditional to check wheter an element is in both list 
I could have also checked for which elements are in both (duplicate values) *)
let set_intersection a b = List.filter (fun x -> (find x b)) a ;;


(* same as set_intersection, simply change the if conditional to be not in the list *)
(* a:[1,2,3,6] b:[2,6] ==== [1,3]   *)
let set_diff a b = List.filter (fun x -> not (find x b)) a ;;


(* helper function *)
let derivative dx f = fun x -> (f (x +. dx) -. f x) /. dx;;

(* && (abs_float((derivative (sqrt epsilon_float) (f x) (x))) < 1.) *)
let rec computed_fixed_point eq f x =
if eq (f x) x then x else
computed_fixed_point eq f (f x) ;;

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal;;


(* need to use pattern matching to access and compare the values. exe: 
			Expr -> (N Expr)
			Expr -> [N Num]
			Expr -> [N Lvalue]
			Expr -> N Incrop, N Lvalue  
	list of tuples which contain usr defined data type and list.
	list of terminal and non-terminal symbols 
 	keeps the N terminals, removes the T*)

let rec filter_right pair = 
	match pair with
    | [] -> []
    | N head :: rest -> head :: filter_right rest
    | T _ :: rest -> filter_right rest;;


(* https://stackoverflow.com/questions/26005545/ocaml-extract-nth-element-of-a-tuple *)
let get_1_2 (a,_) = a ;;	(* get left  *)
let get_2_2 (_,a) = a ;;	(* get right *)


(* let reachable_nonterminal curr lis =
    let filtered = List.filter (fun (a, _) -> a = curr) lis in
    let nonterminals = List.map (fun (_, b) -> filter_right b) filtered in
    curr :: (List.flatten nonterminals);; *)

(* get the N.terminal newly reachable symbols [out here now [],[],[]]*)
let rec reachable_nonterminal curr rules = 
	match rules with
	| [] -> curr
	|head::rest -> if (find (get_1_2 head) curr) then reachable_nonterminal(set_union curr (filter_right(get_2_2 head))) rest
			   else reachable_nonterminal curr rest ;;


(* check for rules one level down [[in here now], [], []]
	no need to pattern match anymore because you are expecting to only see rules in this level *)
let rec deeper_nonterminal curr rules = 
	(* check if the sets given all the reachable non terminals are present, if not add more non terminals. check if they are equal *)
	if equal_sets (reachable_nonterminal curr rules) (reachable_nonterminal (reachable_nonterminal curr rules) rules) then reachable_nonterminal curr rules
	else deeper_nonterminal (reachable_nonterminal (reachable_nonterminal curr rules) rules) rules ;; 


(* will remove all the rules that are cannot be attained. *)
let rec remove_unreachable start rules = 
	List.filter(fun x -> find (get_1_2 x) (deeper_nonterminal [start] rules)) rules ;;


(* after we remove the unreachable ones, all we have left are the reachable rules...  *)
let filter_reachable g = 
	match g with
	| (start, rules) -> (start, remove_unreachable start rules)
	| _ -> g ;;


