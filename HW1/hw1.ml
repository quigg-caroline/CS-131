(* find whether a is in set b *)
let find a b = match b with
	| [] -> false
	| h::t -> if h = a then true else (find a)
;;

(* returns true if set a is a subset of set b *)
let rec subset a b =  match a with
	| []-> true
	| h::t -> if (find h b) then (subset t b) else false


