(* determine if set a is a subset of set b *)
let rec subset a b =  match a with
	| []-> true
	| h::t -> if (List.mem h b) then (subset t b) else false
;;

(* determine if sets a and b are equal*)
let equal_sets a b = (subset a b) && subset(b a);;

(* returns union of sets a b, making sure to remove duplicates*)
(*let rec set_union a b = match a with
	| [] -> b
	| h::t -> if (subset h b) then set_union t b else h::(set_union t b)
;;*)
let set_union a b =  a @ b ;;

(*returns intersection of sets a b*)
let rec set_intersection a b = match a with
	| [] -> []
	| h::t -> if (subset h b) then h::(set_intersection t b) else set_intersection t b
;;

(*return difference between sets a and b*)
let rec set_diff a b = match b with
	| [] -> a
	| h::t -> if (subset h a) then set_diff a t else h::(set_diff a t)
;;

(*compute fixed point, if not found keep recursing*)
let computed_fixed_point eq f x = if eq x (f x) then x else computed_fixed_point eq f (f x) ;;

