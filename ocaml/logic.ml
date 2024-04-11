type bool_expr =
  | Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr;;

let rec generate_pairs n vars =
  let bool = [true; false] in
  let rec aux n acc =
    if n = 1 then List.map (fun b -> (acc @ [b])) bool
    else List.flatten (List.map (fun b -> aux (n-1) (acc @ [b])) bool)
  in List.map (fun b -> List.combine b vars) (aux n []);;

let table2 a b bool_expr = 
  let rec check s = function 
    | [] -> false
    | (b, str) :: t -> if s = str then b else check s t
  in
  let rec compute b = function
    | Var str -> check str b
    | Not b1 -> compute b b1
    | And (b1, b2) -> compute b b1 && compute b b2
    | Or (b1, b2)  -> compute b b1 || compute b b2
  in List.map (fun b -> (b, compute b bool_expr)) (generate_pairs 2 ["a"; "b"]);;

let table vars bool_expr = 
  let rec check s = function 
    | [] -> false
    | (b, str) :: t -> if s = str then b else check s t
  in
  let rec compute b = function
    | Var str -> check str b
    | Not b1 -> compute b b1
    | And (b1, b2) -> compute b b1 && compute b b2
    | Or (b1, b2)  -> compute b b1 || compute b b2
  in List.map (fun b -> (b, compute b bool_expr)) (generate_pairs (List.length vars) vars);;

let rec gray n =
  let digits = ["1"; "0"] in
  let rec aux n acc =
    if n = 1 then List.map (fun b -> acc ^ b) digits
    else List.flatten (List.map (fun b -> aux (n-1) (acc ^ b)) digits)
  in aux n "";;

type 'a tree =
  | Empty
  | Node of 'a * 'a tree * 'a tree * int option;;

(* Sort algorithm from lists exercises *)
let sort f l = 
  let rec insert f acc = function 
    | [] -> [acc]
    | (h :: t) as l -> if compare (f h) (f acc) > 0 then acc :: l else h :: insert f acc t
  in
  let rec aux f = function
    | [] -> []
    | h :: t -> insert f h (aux f t)
  in aux f l;;

(* Naive implementation of https://en.wikipedia.org/wiki/Huffman_coding *)
let huffman l = 
  let initial = List.map (fun a -> Node (a, Empty, Empty, None)) (sort snd l) in
  let get_root (Node (rt, _, _, _)) = rt in
  let update_edge (Node (rt, l, r, _)) n = Node (rt, l, r, Some n) in
  let get_freq t = let _, f = get_root t in f in
  let join a b = 
    let sa, fa = get_root a in
    let sb, fb = get_root b in
    Node ((sa ^ sb, fa + fb), update_edge a 0, update_edge b 1, None)
  in
  let rec freq_tree = function
    | [] -> Empty
    | h :: [] -> h 
    | h :: hs :: t -> freq_tree (sort get_freq ((join h hs) :: t))
  in
  let get_edge (Some n) = string_of_int n in
  let rec aux acc = function 
    | Empty -> []
    | Node ((s, _), Empty, Empty, n) -> [(s, acc ^ get_edge n)]
    | Node (root, l, r, n) -> 
        if None = n then aux acc l @ aux acc r 
        else aux (acc ^ get_edge n) l @ aux (acc ^ get_edge n) r
  in aux "" (freq_tree initial);;

