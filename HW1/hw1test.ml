
(*subset tests*)
let my_subset_test0 = subset [] [] ;;
let my_subset_test1 = subset [] [1] ;;
let my_subset_test2 = subset [1] [1] ;;
let my_subset_test3 = not (subset [1] []);;
let my_subset_test5 = subset [1;2;3] [3;4;2;1;3;2;5;6;5;3;2;1];;
let my_subset_test6 = not (subset [1;1;1;2;3;3;4] [1;2;3]);;
let my_subset_test7 = subset ['a';'b';'c'] ['c';'d';'a';'z';'b'];;
let my_subset_test8 = subset ["hi"] ["hi"; "hello"];;

(*equal_sets tests*)
let my_equal_sets_test0 = equal_sets [] [];;
let my_equal_sets_test1 = equal_sets [1;2;3] [3;2;2;1;1;2;3];;
let my_equal_sets_test2 = equal_sets [1;2;3] [3;2;1];;
let my_equal_sets_test3 = equal_sets [1;2;3] [1;2;3];;
let my_equal_sets_test4 = not (equal_sets [] [1;2]);;
let my_equal_sets_test5 = not (equal_sets [1;2] []);;
let my_equal_sets_test6 = not (equal_sets ["hi"] ["hii"]);;
let my_equal_sets_test7 = equal_sets ["hiiii"] ["hiiii"];;

(*set_union tests*)
let my_set_union_test0 = equal_sets (set_union[1;2;3] [4;5;6]) [1;2;3;4;5;6];;
let my_set_union_test1 = equal_sets (set_union [1;2] [1;2]) [1;2];;
let my_set_union_test2 = equal_sets (set_union [] []) [];;
let my_set_union_test3 = equal_sets (set_union [] [1;2;3]) [1;2;3];;
let my_set_union_test4 = equal_sets (set_union [1;2;2;1] [1;3;4]) [1;2;3;4];;
let my_set_union_test5 = equal_sets (set_union ['1';'4';'3'] ['4';'7']) ['1';'7';'3';'4'];;

(*set_intersection tests*)
let my_set_intersection_test0 = equal_sets (set_intersection [] []) [];;
let my_set_intersection_test1 = equal_sets (set_intersection [1;2] [1;3]) [1];;
let my_set_intersection_test2 = equal_sets (set_intersection [] [1]) [];;
let my_set_intersection_test3 = equal_sets (set_intersection [1;2;3;3;3;5;6;7] [1;5;8;9]) [1;5];;
let my_set_intersection_test4 = equal_sets (set_intersection ["hi";"hello"] ["hi"]) ["hi"];;
let my_set_intersection_test5 = equal_sets (set_intersection [1;2] [3;4]) [] ;;

(*set_diff tests*)
let my_set_diff_test0 = equal_sets (set_diff [] []) [];;
let my_set_diff_test1 = equal_sets (set_diff [1;2;3] [] ) [1;2;3];;
let my_set_diff_test2 = equal_sets (set_diff [] [1;2]) [] ;;
let my_set_diff_test3 = equal_sets (set_diff [1;2;3;4;5;5;6;7] [7;5;6]) [1;2;3;4];;
let my_set_diff_test4 = equal_sets (set_diff [5;6;3;4;5] [3;4;5;6]) [] ;; 

(*computed_fixed_point tests*)
let my_computed_fixed_point_test0 = (computed_fixed_point (=) (fun x -> x*x ) 1) = 1;;
let my_computed_fixed_point_test1 = (computed_fixed_point (=) (fun x -> x*x ) (-1) ) = 1;;
let my_computed_fixed_point_test2 = (computed_fixed_point (=) (fun x -> x/2 ) 100) = 0;;
let my_computed_fixed_point_test3 = (computed_fixed_point (fun a b -> a+9=b) (fun x-> x + x/2) 2) = 19;;

(*computed_periodic_point tests*)
let my_computed_periodic_point_test0 = (computed_periodic_point (=) (fun x-> x*x) 1 1) = 1 ;;
let my_computed_periodic_point_test1 = (computed_periodic_point (=) (fun x-> x*x) 0 15) = 15 ;;
let my_computed_periodic_point_test2 = (computed_periodic_point (=) (fun x-> x *. x -. 1.) 2 0.5) = -1.;;

(*while_away tests*)
let my_while_away_test0 = (while_away ((-) 2) ((<) 10) 12) = [12];;
let my_while_away_test1 = (while_away (fun x->x-2) ( (<) 0) 12) = [12;10;8;6;4;2] ;;
let my_while_away_test2 = (while_away (fun x -> x) (fun x -> x = 10) 2) = [];;
let my_while_away_test3 = (while_away (fun x -> x*x) ( (>) 26) 2) = [2;4;16];;
let my_while_away_test3 = (while_away (fun x -> x+2) (fun x -> x+2 < 12) 2) = [2;4;6;8];;

(*rld_decode tests*)
let my_rle_decode_test0 = (rle_decode [1,3; 2,2]) = [3;2;2];;
let my_rle_decode_test1 = (rle_decode [0,2; 0,1]) = [];;
let my_rle_decode_test2 = (rle_decode [])= [];;
let my_rle_decode_test3 = (rle_decode [1,0]) = [0];;
let my_rle_decode_test4 = (rle_decode [3,"a"; 2,"b"; 0,"c"; 1,"d"]) = ["a";"a";"a";"b";"b";"d"];;

(*filter_blind_alleys tests*)
let test_grammar1 = ("S",
	[
		"S", [T"a"];
	]
);;

let test_grammar2 = ("S",
	[
		"S", [N"T"];
		"T", [T"a"]
	]
);;

let test_grammar3 = ("S",
	[
		"S", [N"T"];
		"T", [T"a"];
		"T", [N"D"]
	]
);;

let my_filter_blind_alleys_test0 = filter_blind_alleys(test_grammar1) = test_grammar1;;
let my_filter_blind_alleys_test1 = filter_blind_alleys(test_grammar2) = test_grammar2;;
let my_filter_blind_alleys_test2 = filter_blind_alleys(test_grammar3) = test_grammar2;;
