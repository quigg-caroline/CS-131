(*symbol type*)
type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

(* determine if set a is a subset of set b *)
let rec subset a b =  match a with
	| []-> true
	| h::t -> if (List.mem h b) then (subset t b) else false
;;

(* determine if sets a and b are equal*)
let equal_sets a b = (subset a b) && (subset b a);;

(* returns union of sets a b, making sure to remove duplicates*)
let set_union a b =  a @ b ;;

(*returns intersection of sets a b*)
let rec set_intersection a b = match a with
	| [] -> []
	| h::t -> if (subset [h] b) then h::(set_intersection t b) else set_intersection t b
;;

(*return difference between sets a and b*)
let rec set_diff a b = match a with
	| [] -> []
	| h::t -> if (subset [h] b) then set_diff t b else h::(set_diff t b)
;;

(*compute fixed point, if not found keep recursing*)
let rec computed_fixed_point eq f x = if eq x (f x) then x else computed_fixed_point eq f (f x) ;;

let rec computed_periodic_point eq f p x = match p with
	| 0 -> x
	| _ -> if eq x ( f (computed_periodic_point eq f (p - 1) (f x))) then x else (computed_periodic_point eq f p (f x))
;; 

(*returns longest list s.t. all the elements are x; s x; s (s x); etc. and statisfy p*)
let rec while_away s p x =
	if not (p x) then []
	else x::(while_away s p (s x))
;;

(*decode list with run-length encoding*)
let rec rle_decode lp = match lp with
	| [] -> []
	| (len,v)::t -> if len<1 then rle_decode t else v::rle_decode ((len - 1, v)::t )
;;

(*filter out blind-alleys in a grammar*)

(*goal is to build backwards more or less..*)

(*determine whether this is a terminal symbol (i.e eventually terminates) *)
let rec sym_terminal sym rule_list=  match sym with
	| T exp -> true
	| N exp -> if List.exists(fun x-> (fst x) = exp) rule_list then true else false
;;

(*determine whether all the subrules in a right hand side are terminal*)
let rec rhs_terminal rhs rule_list = match rhs with
	| [] -> true
	| sym::t -> if sym_terminal sym rule_list then rhs_terminal t rule_list else false
;;

(*determine the list of terminal rules *)
let rec rules_terminal rules rule_list = match rules with
	| [] -> rule_list
	| rule::t -> if (rhs_terminal (snd rule) rule_list) && (not (subset [rule] rule_list))
	then rules_terminal t (rule::rule_list) else rules_terminal t rule_list
;;

(*computer rules_terminal until it doesn't change -> as we add more rules to rule_list our good rules
might change *)
let rec terminal_rules rules =
	computed_fixed_point equal_sets (rules_terminal rules) []
;;

(*filter out the nonterminal rules*)
let filter_blind_alleys g = match g with
	| (start,rules) -> (start,  List.filter (fun x -> List.exists (fun y -> y=x ) (terminal_rules rules)) rules )
;; 



