(* set functions *)
let subset_test0 = subset [] [1;2;3]
let subset_test1 = not(subset [1;2;3] [])
let subset_test2 = subset [1;2] [1;2;2]
let subset_test3 = subset [1;2;3] [1;2;3;4]

let equal_sets0 = equal_sets [1;1;4;1;4;1] [1;1;4]
let equal_sets1 = equal_sets [] []
let equal_sets2 = not(equal_sets [1;2;3] [4;5;6])



let set_union_test0 = equal_sets (set_union [1;2] [3;4]) [1;2;3;4]
let set_union_test1 = equal_sets (set_union [] [1;2;3]) [1;2;3]
let set_union_test2 = not(equal_sets (set_union [6] [1;2;3]) [1;2;3])

let set_intersection_test0 = equal_sets (set_intersection [3;1;0] [1;2;3;0]) [1;3;0]
let set_intersection_test1 = not(equal_sets (set_intersection [] [1;2;3;0]) [1;2;3;0])


(* computed fix point *)
let computed_fixed_point_test0 = computed_fixed_point (=) (fun x -> ((x*x) - (3*x) + 4) ) 2 = 2


(* filter_reachable *)
type subter = 
|A|B|C|D|E

let awksub_rules =
   [Expr, [T"("; N Expr; T")"];
    Expr, [N Num];
    Expr, [N Expr; N Binop; N Expr];
    Expr, [N Lvalue];
    Expr, [N Incrop; N Lvalue];
    Expr, [N Lvalue; N Incrop];
    Lvalue, [T"$"; N Expr];
    Incrop, [T"++"];
    Incrop, [T"--"];
    Binop, [T"+"];
    Binop, [T"-"];
    Num, [T"0"];
    Num, [T"1"];
    Num, [T"2"];
    Num, [T"3"];
    Num, [T"4"];
    Num, [T"5"];
    Num, [T"6"];
    Num, [T"7"];
    Num, [T"8"];
    Num, [T"9"]]
let grammar = Expr, awksub_rules 

let filter_test = filter_reachable grammar = grammar