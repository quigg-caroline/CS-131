let accept_all derivation string = Some (derivation, string) ;;
let accept_empty_suffix derivation = function
   | [] -> Some (derivation, [])
   | _ -> None
;;

type text_speak_nonterms = 
	| Lol | Jk | Gdi | Tfti | Idk 

let rec has_laughs = function
	| [] -> false
	| (Lol,_)::_ -> true
	| _::rules -> has_laughs rules
;;

let accept_no_laughs rules frag = if has_laughs rules
	then None
	else Some(rules, frag)
;; 


let text_speak_grammar = 
	( Idk,
		function
			| Idk -> [ [N Lol; N Jk; N Idk]; [N Lol];[N Gdi]] 
			| Lol -> [ [T "haha"] ; [N Gdi]; ]
			| Jk ->  [ [T "just kidding"; N Gdi] ; [T "just kidding"]]
			| Gdi -> [ [N Tfti ; T "gosh darn it"]; [N Tfti] ]
			| Tfti -> [ [T "thanks"] ; [T "for the"] ; [T "invite"] ]
	)
;; 

let test_1 = ( (parse_prefix text_speak_grammar accept_empty_suffix ["haha"; "just kidding"; "thanks"]) = 
	Some( [ (Idk, [N Lol; N Jk; N Idk] ); (Lol, [T"haha"]) ; (Jk, [T "just kidding"]) ; (Idk, [N Lol]) ; (Lol, [N Gdi]); (Gdi, [N Tfti]); (Tfti, [T "thanks"])], [] ) );;

let test_2 = ((parse_prefix text_speak_grammar accept_no_laughs ["thanks";"gosh darn it";"just kidding";"haha"]) =
		Some([(Idk, [N Gdi]); (Gdi, [N Tfti; T"gosh darn it"]) ; (Tfti, [T "thanks"])],["just kidding"; "haha"]));;