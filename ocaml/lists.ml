let rec add = function
  | [] -> None
  | h :: [] -> Some(h)
  | h :: t -> add(t);;

let rec last_two = function
  | [] -> None
  | h :: t :: [] -> Some(h, t)
  | h :: t -> last_two(t);;

let rec at n = function
  | [] -> None
  | h :: t -> if n = 1 then Some(h) else at (n-1) t;;

let rec length = function
  | [] -> 0
  | h :: t -> 1 + length(t);;

let rec rev = function
  | [] -> []
  | h :: t -> (rev t) @ [h];;

let rec is_palindrome l = l = rev(l);;

type 'a node =
  | One of 'a 
  | Many of 'a node list;;

let rec flatten = function
  | [] -> []
  |	One h	 :: t -> h :: flatten t
  |	Many h :: t -> flatten h @ flatten t;;

let rec compress = function
  | [] -> []
  | a :: [] -> [a] 
  | h :: t -> if h <> List.hd t then h :: compress t else compress t;;

(* Need a function for each dimension? *)
let pack l = 
  let rec temp acc = function
    | [] -> []
    | h :: [] -> [acc]
    | h :: t -> 
        if h <> List.hd t then [acc] @ temp [List.hd t] t 
        else temp (h :: acc) t
  in temp [] l;;

let encode l = 
  let rec temp (count, item) = function
    | [] -> []
    | h :: [] -> 
        if h <> item then (count, item) :: [(1, h)]
        else [(count+1, item)]
    | h :: t -> 
        if h <> item then [(count, item)] @ temp (1, h) t 
        else temp (count+1, item) t
  in match l with
    | [] -> []
    | h :: t -> temp (1, h) t;;

type 'a rle =
  | One of 'a
  | Many of int * 'a;;

(* Modified *)
let encode l =  
  let rec aux count = function
    | [] -> []
    | h :: [] -> if count = 0 then [One h] else [Many (count+1, h)]
    | a :: (b :: _ as t) -> 
        if a <> b then 
          let acc = if count = 0 then One a
          else Many (count + 1, a) 
          in [acc] @ aux 0 t 
        else aux (count+1) t
  in aux 0 l;;

let rec decode = function
  | [] -> []
  | One item :: t -> item :: decode t
  | Many (count,item) :: t -> 
      if count = 0 then decode t 
      else item :: decode (Many (count - 1, item) :: t);;

let encode lst = 
  let rle count x = if count = 0 then One x else Many (count + 1, x) in 
  let rec aux count acc = function
    | [] -> []
    | a :: [] -> [rle count a]
    | a :: (b :: _ as t) -> if a = b then aux (count + 1) acc t else aux 0 ((rle count a) :: acc) t
  in aux 0 [] lst;; 

let rec duplicate = function
  | [] -> [] 
  | h :: t -> h :: h :: duplicate t;;

let replicate l n = 
  let rec aux i = function
    | [] -> []
    | (h :: t as lst) -> if i = 0 then aux n t else h :: aux (i - 1) lst
  in aux n l;;

(* "replicate" Official solution *)
let replicate l n = 
  let rec prepend count item acc = if count = n then item :: acc else prepend (count+1) item (item :: acc) in
  let rec aux acc = function
    | [] -> acc
    | h :: t -> aux (prepend 1 h acc) t
  in aux [] (List.rev l);;

let drop l n = 
  let rec aux i = function
    | [] -> []
    | h :: t -> if i = 1 then aux n t else h :: aux (i - 1) t
  in aux n l;;

let split l n = 
  let rec aux acc i = function
    | [] -> (List.rev acc, [])
    | h :: t -> if i = 0 then (List.rev acc, h :: t) else aux (h :: acc) (i - 1) t
  in aux [] n l;;

let rec slice l a b =
  match l with
    | [] -> []
    | h :: t -> 
        if a = 0 then 
          if b = 1 then []
          else h :: slice t 0 (b-1)
        else slice t (a-1) b;;

let rotate l n = 
  let a, b = split l (if n > 0 then n else n + (List.length l) - 1) in b @ a;;

let rec remove_at n = function
  | [] -> []
  | h :: t -> if n = 0 then t else h :: remove_at (n-1) t;;

let rec insert_at item n = function
  | [] -> []
  | h :: t -> if n = 0 then item :: t else h :: insert_at item (n-1) t;;

let rec range a b = 
  if a = b then [a] 
  else if a > b then a :: range (a-1) b
  else a :: range (a+1) b;;

(* My first solution to this problem didn't involved selecting *distinct* elemnts. 
  (Partially because I thought the quesiton didn't required it given the context.) *)
let rand_select l n = 
  let _ = Random.self_init in
  let rec extract acc n = function
    | [] -> raise Not_found
    | h :: t -> if n = 0 then h, acc @ t else extract (h :: acc) (n-1) t
  in 
  let extract_aux l = extract [] (Random.int ((List.length l) - 1)) l in
  let rec aux l n = 
    if n = 0 then [] 
    else let h, l = extract_aux l in h :: aux l (n-1)
  in aux l n;;

let lotto_select n r = rand_select (range 0 r) n;;

let permutation = function
  | [] -> []
  | l -> rand_select l ((List.length l) - 1);;

let rec extract n l = 
  (* [[]] is considered a list with one element, i.e. "map" function will be applied. *)
  if n = 0 then [[]]
  else match l with
    | [] -> []
    | h :: t -> List.map (fun l -> h :: l) (extract (n-1) t) @ extract n t;;

(* Spent a lot of time thinking... got close but couldn't think of an answer so I cheated (kinda. spent a lot of time trying to understand the oficial answer) *)
let rec group l n = 
  let initial = List.map (fun n -> n, []) n in
  let prepend p list = 
    let temp l acc = l :: acc in
    let rec aux temp acc = function
      | [] -> temp [] acc
      | (n, l) as h :: t -> 
          let acc = 
            if n > 0 then temp ((n-1, p :: l) :: t) acc
            else acc in aux (fun l acc -> temp (h :: l) acc ) acc t
    in aux temp [] list
  in
  let rec aux = function
    | [] -> [initial]
    | h :: t -> List.concat (List.map (prepend h) (aux t))
  in let filtered = List.filter (List.for_all (fun (x, _) -> x = 0)) (aux l) in
  List.map (List.map snd) filtered;;

