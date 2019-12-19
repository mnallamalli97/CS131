type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal;;

let accept_all str = Some str
let accept_empty_suffix = function
   | _::_ -> None
   | x -> Some x

(* part 1 *)

(* Takes all rules that match given nonterminal and returns them as a list. *)
let rec take nt rules =
  match rules with
  | [] -> []
  | (nt2,rule_list)::tail when nt = nt2 -> rule_list::(take nt tail)
  | _::tail -> take nt tail

(* Takes a grammar and creates a function for transforming a list of rules
   into a function returning filtered list of rules that match given
   nonterminal. *)
let convert_grammar gram_list =
  let (start,rules) = gram_list in
  let res_fun = fun nt -> take nt rules in
  (start, res_fun)

(* Function uses recursion and folding of the same level nodes to make it
   into a list (by List.append as folding function). *)
let rec parse_tree_leaves tree =
  match tree with
  | Leaf t -> [t]
  | Node (nt,lst) ->
      List.fold_right (fun item acc -> List.append (parse_tree_leaves item) acc)
          lst []


(* part 2 *)

(* For a given fragment the function "eats" the very first string if it is
   the same as parameter str, otherwise it returns None. *)
let eat str frag =
  match frag with
  | [] -> None
  | h::tail when h = str -> Some tail
  | _ -> None

(* Simple extractor of the result - given full parser result it extracts
   only the AST from it. *)
let take_tree = function
  | None -> None
  | Some (tree, _, _) -> Some tree

(* Simple extractor of the result - given full parser result it extracts
   only the remaining fragment from it. *)
let take_frag = function
  | None -> None
  | Some (_, frag, _) -> Some frag

(* Node builder. The function gets a nonterminal (nt) and resulting
   tree. If it is None then is returns None as well, otherwise
   it creates a Node with reversed list of subnodes. Note that it needs
   to be reversed because in parsing it is generated in reversed order
   using tail recursion *)
let build_tree nt res_opt =
  match res_opt with
  | None -> None
  | Some (tree_lst, leftover, tree_rec) ->
      Some (Node (nt, List.rev tree_lst), leftover, tree_rec)

(* The function that wraps acceptor defined in the task into the acceptor
   that is expected in parser. It takes a fragment and returns None
   if original acceptor returns None. If original acceptor returns the fragment
   then it wraps the result into a triple (AST list, frag, AST list). *)
let wrap_acceptor acceptor = fun frag ->
  match acceptor frag with
  | None -> None
  | Some x -> Some ([], x, [])

(* The function runs the acceptor and puts the results as third part of
   the returned triple concatenating it with recursively returned tree_lst_rec.
   Current accumulator becomes the current result. *)
let build_bottom_level_result acceptor acc = fun frag ->
  match acceptor frag with
  | None -> None
  | Some (tree_lst, leftover, tree_lst_rec) ->
      Some (acc, leftover, tree_lst::tree_lst_rec)

(* The function that parses one level of grammar, i.e. [N Term; N Binop; N Expr]
   where all items need to be parsed correctly, one by one. It parses 
   all items on the list and for all nonterminals (that can not be parsed
   immedately) it calls next_parser to parse given nonterminal from scratch.

   Notice that we parse tokens (within fragment) left to right. When we have
   a nonterminal item to pase (i.e. N Term) then we need to parse it recursively
   and after this is finished we may continue with next one (i.e. N Binop).
   This continuation is injected into further parsing as an acceptor function
   (new_acceptor), so when our recursive parsing of N Term is finished then
   it calls this continuation (new_acceptor) to continue parsing N Binop
   and N Expr.

   In shorter words: continuation of computation is injected into recursive
   call as the acceptor function (new_acceptor).

   The function parses one level with sort of logical AND (all items need
   to be parsed). It needs to collect all the parsed items, so it stores
   them in the accumulator (acc). This accumulator bound into a node with
   build_tree or stored in the third result parameter of
   Some (parsed_AST_list, fragment_left, parsed_AST_list_by_acceptor).
   *)
let rec parse_one next_parser acc rule acceptor frag = (* AND *)
  match rule with
  | [] -> build_bottom_level_result acceptor acc frag
  | (T str)::tail ->
      let eaten = eat str frag in
      (match eaten with
      | None -> None
      | Some leftover ->
          parse_one next_parser ((Leaf str)::acc) tail acceptor leftover)
  | (N new_nt)::tail ->
      let new_acceptor = fun fr ->
        parse_one next_parser [] tail acceptor fr
      in
      let sub_res = next_parser new_nt new_acceptor frag in
      match sub_res with
      | None -> None
      | Some (tree, leftover, []) -> Some (tree::acc, leftover, [])
      | Some (tree, leftover, th::ttail) -> Some (th @ (tree::acc), leftover, ttail)

(* The function that parses the alternatives within the grammar.
   Example: [N Term; N Binop; N Expr] OR [N Term].
   It gets the list of alternatives that may be parsed and tries them one by one.
   As soon as any of these alternatives work then the result is returned.

   Note that it uses the function parse_one for parsing each of the alternatives. *)
let rec parse_r next_parser rules acceptor frag = (* OR *)
  match rules with
  | [] -> None
  | h::tail ->
      let res = parse_one next_parser [] h acceptor frag in
      match res with
      | None -> parse_r next_parser tail acceptor frag
      | _ -> res

(* Top level function that parses one nonterminal (nt) given the function
   from the grammar (rule_fn). It gets all the alternatives for given
   nonterminal with (rule_fn nt) then passes it into parse_r function
   and finally builds a Node with the result (or None if it was not parsed). *)
let rec parse rule_fn nt acceptor frag =
  build_tree nt (parse_r (parse rule_fn) (rule_fn nt) acceptor frag)

(* The desired function that takes a grammar and calls parse function
   with the acceptor that accepts only empty suffix. This means that whole
   fragment needs to be parsed. *)
let make_parser gram frag =
  let (start, rule_fn) = gram in
  take_tree (parse rule_fn start (wrap_acceptor accept_empty_suffix) frag)

(* The another desired function that should parse the fragment. The only
   differences are: it does not care about AST and it may accept several
   suffixes. Additional parameter acceptor is passed into the parser
   to accept only desired fragments that are left after parsing. *)
let make_matcher gram = fun acceptor frag ->
  let (start, rule_fn) = gram in
  take_frag (parse rule_fn start (wrap_acceptor acceptor) frag)
