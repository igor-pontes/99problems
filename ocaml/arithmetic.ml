let is_prime n = 
  let sqrt = int_of_float (sqrt ( float_of_int n )) in
  let rec aux n f = 
    if n mod f = 0 then false
    else if f = sqrt+1 then true else aux n (f+1) in
  n <> 1 && aux n 2 || n = 2;;

let gcd a b = 
  let rec aux n r =
    if r = 0 then n
    else if r > n then 1 else aux r (n mod r)
  in
  if a > b then aux a b else aux b a;;

(* Coprime if GCD = 1*)
let coprime a b = gcd a b = 1;;

(* Euler's totient fucntion outputs the number r of positive integers that are coprime to m. (1 <= r < m)*)
let rec phi m = 
  let rec aux r = 
    if r = 1 then 1 
    else if coprime m r then 1 + aux (r-1) else aux (r-1)
  in aux (m-1);;

let factors n = 
  let rec aux n f = 
    if n = 1 then [] else
    if n mod f = 0 then f :: aux (n/f) f else aux n (f+1) 
  in aux n 2;;

(* Encode from list exercises *)
let encode l = 
  let rec temp (count, item) = function
    | [] -> [(count, item)]
    | h :: t -> 
        if h <> item then (count, item) :: temp (1, h) t 
        else temp (count+1, item) t
  in match l with
    | [] -> []
    | h :: t -> temp (1, h) t;;

let ( *** ) b p = 
  let rec aux acc p = if p = 0 then acc else aux (acc*b) (p-1)
  in aux 1 p;;

let phi_improved n = 
  let rec aux = function
    | [] -> 1
    | (m, p) :: t -> ((p-1) * (p *** (m-1))) * aux t
  in aux (encode (factors n));;

let timeit f n = 
  let start = Sys.time () in
  let _ = f n in Sys.time () -. start;;

let rec all_primes a b = 
  if a = b then [] 
  else if is_prime a then a :: all_primes (a + 1) b
  else all_primes (a + 1) b;;

let goldbach n = 
  let rec compute a = function
    | [] -> None
    | h :: t -> if a + h = n then Some(a, h) else compute a t
  in
  let rec aux = function
    | [] -> 0, 0
    | h :: t -> 
      match compute h t with
        | Some (a, b) -> (a, b)
        | None -> aux t
  in aux (all_primes 2 n);;

let rec goldbach_list a b = 
  if a > b then []
  else if a mod 2 = 0 then (a, goldbach a) :: goldbach_list (a+1) b
  else goldbach_list (a+1) b;;

let rec goldbach_limit a b c = 
  let check (_, (a, b)) = a > c && b > c in
  List.filter check (goldbach_list a b);;

