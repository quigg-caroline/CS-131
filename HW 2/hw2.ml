type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal
;;

(*return list of subrules which match to a nonterminal*)
let rec test rules lhs = match rules with
	| [] -> []
	| (nonterm,rhs)::t -> if (nonterm = lhs) then rhs::(test t lhs) else test t lhs
;;

(*convert hw1 grammar to hw2 grammar*)
(*return tuple of starting symbol and a function matching non-terminals to subrules*)
let convert_grammar gram1 = (fst gram1) , test (snd gram1) ;;


let rec deriv_pref matching start_sym rules acceptor deriv frag  = match rules with
	| [] -> None (*there are no rules left for this start_sym*)
	| rule::alt_rules -> match (match_rule matching acceptor rule (deriv@[start_sym,rule]) frag) with 
		| None -> deriv_pref (matching) start_sym alt_rules acceptor deriv frag
		| Some x -> Some x
and match_rule matching acceptor rule deriv frag = match rule with
	| [] -> acceptor deriv frag (**)
	| _ -> match frag with
		| [] -> None (*there is still part of the rule left but none of the frag left*)
		| _ -> (match_term (matching) acceptor deriv rule frag)
and match_term matching acceptor deriv rule frag = match rule with
	| [] -> None  (*there are no more symbols in the rule*)
	| T term::t -> if term = (List.hd(frag)) then match_rule (matching) acceptor t deriv (List.tl(frag)) else None
	| N term::t -> deriv_pref matching term (matching term) (match_rule matching acceptor t) deriv frag 
	(*update acceptor to preseve right hand of the rule*)
;;

let parse_prefix gram acceptor frag = match gram with
	| (start,matching) -> deriv_pref (matching) start (matching start) acceptor [] frag
;; 







