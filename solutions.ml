
(* PROBLEM 1 *)
let rec last = function
    | [] -> None
    | x::[] -> Some x
    | x::xs -> last xs
;;

(* PROBLEM 2 *)
let rec last_two = function
    | [] -> None
    | x::[] -> None
    | x::y::[] -> Some (x,y)
    | x::xs -> last_two xs
;;

(* PROBLEM 3 *)
let rec at k = function
    | [] -> None
    | x::xs -> if k=0 then Some x else at (k-1) xs
;;

(* PROBLEM 4 *)
let length lst = 
    let rec aux acc lst =
        match lst with
        | [] -> acc
        | x::xs -> aux (acc+1) xs
    in aux 0 lst
;;

(* PROBLEM 5 *)
let rev lst =
    let rec aux acc = function
        | [] -> acc
        | x::xs -> aux (x::acc) xs
    in aux [] lst
;;

(* PROBLEM 6 *)
let is_palindrome lst =
    lst = rev lst
;;

(* PROBLEM 7 *)
type 'a node =
    | OneNode of 'a
    | ManyNode of 'a node list
;;

let flatten lst =
    let rec aux acc = function
        | [] -> acc
        | OneNode x :: xs -> aux (x::acc) xs
        | ManyNode xs :: xss -> aux (aux acc xs) xss
    in rev(aux [] lst)
;;

(* PROBLEM 8 *)
let rec compress = function
    | x :: (y :: _ as tail) -> if x=y then compress tail else x::(compress tail)
    | smtgelse -> smtgelse
;;

(* PROBLEM 9 *)
let pack lst =
    let rec aux acc1 acc2 lst =
        match acc1, acc2, lst with
        | [], acc2, [] -> acc2
        | acc1, acc2, [] -> acc1::acc2
        | [], acc2, x::xs -> aux [x] acc2 xs
        | y::_, acc2, x::xs -> if x = y
                               then aux (x::acc1) acc2 xs
                               else aux [x] (acc1::acc2) xs
    in rev(aux [] [] lst)
;;

(* PROBLEM 10 *)
let encode lst =
    let rec aux acc packed =
        match packed with
        | [] -> acc
        | []::xs -> aux acc xs
        | (x::xs as hd)::xss -> aux (((length hd), x)::acc) xss
    in rev(aux [] (pack lst))
;;

(* PROBLEM 11 *)
type 'a rle =
    | OneRLE of 'a
    | ManyRLE of (int * 'a);;

let encode2 lst =
    let rec aux acc packed =
        match packed with
        | [] -> acc
        | []::xs -> aux acc xs
        | [x]::xs -> aux ((OneRLE x)::acc) xs
        | (x::xs as hd)::xss -> aux ((ManyRLE ((length hd), x))::acc) xss
    in rev(aux [] (pack lst))
;;

(* PROBLEM 12 *)
let decode lst =
    let rec aux acc lst =
        match lst with
        | [] -> acc
        | ((OneRLE x)::xs) -> aux (x::acc) xs
        | ((ManyRLE (2, x))::xs) -> aux (x::acc) ((OneRLE x)::xs)
        | ((ManyRLE (counter, x))::xs) -> aux (x::acc) ((ManyRLE ((counter-1), x))::xs)
    in rev(aux [] lst)
;;

(* PROBLEM 13 *)
let encode3 lst =
    let rec aux acc lst =
        match acc, lst with
        | acc, [] -> acc
        | [], x::xs -> aux [OneRLE x] xs
        | (OneRLE element)::tail, x::xs ->
                if x=element
                then aux ((ManyRLE (2, element))::tail) xs
                else aux ((OneRLE x)::acc) xs
        | (ManyRLE (counter, element))::tail, x::xs ->
                if x=element
                then aux ((ManyRLE ((counter+1), element))::tail) xs
                else aux ((OneRLE x)::acc) xs
    in rev(aux [] lst)
;;

(* PROBLEM 14 *)
let duplicate lst =
    let rec aux acc lst =
        match lst with
        | [] -> acc
        | x::xs -> aux (x::(x::acc)) xs
    in rev(aux [] lst)
;;

(* PROBLEM 15 *)
let replicate lst num =
    let rec aux acc counter lst =
        match counter, lst with
        | _, [] -> acc
        | 1, x::xs -> aux (x::acc) num xs
        | counter, x::xs -> aux (x::acc) (counter-1) lst
    in rev(aux [] num lst)
;;

(* PROBLEM 16 *)
let drop lst num =
    let rec aux acc counter lst =
        match counter, lst with
        | _, [] -> acc
        | 1, x::xs -> aux acc num xs
        | counter, x::xs -> aux (x::acc) (counter-1) xs
    in rev(aux [] num lst)
;;

(* PROBLEM 17 *)
let split lst num =
    let rec aux acc counter lst =
        match counter, lst with
        | _, [] -> ((rev acc), [])
        | 0, lst -> ((rev acc), lst)
        | counter, x::xs -> aux (x::acc) (counter-1) xs
    in aux [] num lst
;;

(* PROBLEM 18 *)
let slice lst start finish =
    let rec aux acc start finish lst =
        match start, finish, lst with
        | _, _, [] -> acc
        | _, b, x::xs when b < 0 -> acc
        | a, b, x::xs when a > 0 -> aux acc (a-1) b xs
        | a, b, x::xs -> aux (x::acc) (a-1) (b-1) xs
    in rev(aux [] start (finish-start) lst)
;;

(* PROBLEM 19 *)
let rotate lst num =
    let rec aux acc counter lst =
        match counter, lst with
        | 1, x::xs -> xs @ (rev (x::acc))
        | counter, x::xs -> aux (x::acc) (counter-1) xs
        | _, [] -> aux [] (length acc) acc (* Catch all, shouldnt be hit *)
    in match num with
    | num when num < 0 -> aux [] ((length lst) + (num mod (length lst))) lst
    | num -> aux [] (num mod (length lst)) lst
;;

(* PROBLEM 20 *)
let remove_at num lst =
    let rec aux acc counter lst =
        match counter, lst with
        | counter, lst when counter >= (length lst) -> lst
        | 0, x::xs -> (rev acc) @ xs
        | counter, x::xs -> aux (x::acc) (counter-1) xs
        | _, [] -> (rev acc)
    in aux [] num lst
;;
    
(* PROBLEM 21 *)
let insert_at elem idx lst =
    let rec aux elem acc counter lst =
        match counter, lst with
        | 0, lst -> (rev acc) @ (elem::lst)
        | counter, x::xs -> aux elem (x::acc) (counter-1) xs
        | _, [] -> (rev (elem::acc))
    in aux elem [] idx lst
;;

(* PROBLEM 22 *)
let range a b =
    let rec aux acc a b =
        match a, b with
        | a, b when a <= b -> aux (a::acc) (a+1) b
        | _, _ -> (rev acc)
    in match a, b with
    | a, b when a > b -> (rev (aux [] b a))
    | _, _ -> aux [] a b
;;

(* PROBLEM 23 *)
let rand_select lst num =
    let rec aux acc lst counter =
        match counter with
        | counter when counter <= 0 -> acc
        | _ ->  let idx = (Random.int (length lst))
                in let elem = (at idx lst)
                   in match elem with
                   | None -> acc
                   | Some x -> aux (x::acc) (remove_at idx lst) (counter-1)
    in aux [] lst num
;;

(* PROBLEM 24 *)
let lotto_select num bound =
    rand_select (range 1 bound) num
;;

(* PROBLEM 25 *)
let permutation lst =
    let rec aux acc lst =
        match lst with
        | [] -> acc
        | lst -> let idx = (Random.int (length lst))
                 in let elem = (at idx lst)
                    in match elem with
                    | None -> acc
                    | Some x -> aux (x::acc) (remove_at idx lst)
    in aux [] lst
;;

(* PROBLEM 26 *)
let extract num lst = 
    let rec aux left right =
        match left, right with
        | left, _ when (length left) = num -> [left]
        | left, [] -> []
        | left, x::xs -> (aux (x::left) xs) @ (aux left xs)
    in aux [] lst
;;

(* PROBLEM 27 *)
let inside elem lst =
    let rec aux elem lst =
        match lst with
        | [] -> false
        | x::xs when x = elem -> true
        | x::xs -> aux elem xs
    in aux elem lst
;;

let complement set subset = 
    let rec aux acc set =
        match set with 
        | [] -> acc
        | x::xs when (inside x subset) -> aux acc xs
        | x::xs -> aux (x::acc) xs
    in rev (aux [] set)
;;

let unpack lstlst =
    let rec aux acc lstlst =
        match lstlst with
        | [] -> acc
        | []::xss -> aux acc xss
        | (x::[])::xss -> aux (x::acc) xss
        | (x::xs)::xss -> aux (x::acc) (xs::xss)
    in (rev (aux [] lstlst))
;;

let map f lst =
    let rec aux acc lst =
        match lst with
        | [] -> acc
        | x::xs -> aux ((f x)::acc) xs
    in (rev (aux [] lst))
;;

let map2 f lst1 lst2 =
    let rec aux acc lst1 lst2 =
        match lst1, lst2 with
        | [], _ -> acc
        | _, [] -> acc
        | x::xs, y::ys -> aux ((f x y)::acc) xs ys
    in rev (aux [] lst1 lst2)
;;

let group elements sizes =
    let rec aux acc sizes =
        match sizes with
        | [] -> acc
        | x::xs ->
            let sets = map unpack acc in
            let complements = map (complement elements) sets in
            let subsets = map (extract x) complements in
            let acc2 = unpack (map2 (fun x y -> map (fun a -> a::y) x) subsets acc) in
                aux acc2 xs
    in aux [[]] sizes
;;

(* PROBLEM 28 *)
let merge comp lstA lstB =
    let rec aux acc lstA lstB =
        match lstA, lstB with
        | [], [] -> (rev acc)
        | [], lstB -> (rev acc) @ lstB
        | lstA, [] -> (rev acc) @ lstA
        | x::xs, y::ys when (comp x y) -> aux (x::acc) xs lstB
        | lstA, y::ys -> aux (y::acc) lstA ys
    in aux [] lstA lstB
;;

let rec sort comp lst =
    match lst with
    | [] -> []
    | x::[] -> [x]
    | lst ->
            let (left, right) = (split lst ((length lst) / 2))
            in (merge comp (sort comp left) (sort comp right))
;;

let length_sort lstlst =
    sort (fun a b -> (length a) < (length b)) lstlst
;;

let key_inc key keyvalues =
    let rec aux acc keyvalues =
        match keyvalues with
        | [] -> ((key, 1)::acc)
        | (a_key, value)::xs when a_key = key -> ((key, (value + 1))::acc) @ xs
        | x::xs -> aux (x::acc) xs
    in aux [] keyvalues
;;

let rec key_get key keyvalues =
    match keyvalues with
    | [] -> 0
    | (a_key, value)::xs when a_key = key -> value
    | _::xs -> key_get key xs
;;

let length_histogram lstlst =
    let rec aux acc lstlst =
        match lstlst with
        | [] -> acc
        | x::xs -> aux (key_inc (length x) acc) xs
    in aux [] lstlst
;;

let frequency_sort lstlst =
    let histogram = (length_histogram lstlst) in
    sort (fun a b -> key_get (length a) histogram < key_get (length b) histogram) lstlst
;;

(* PROBLEM 29 *)

(* index (fun b -> b) [true; false; false; true; false; true] = [0; 3; 5] *)
let index f lst =
    let rec aux acc counter lst =
        match lst with
        | [] -> rev acc
        | x::xs when (f x) -> aux (counter::acc) (counter + 1) xs 
        | x::xs -> aux acc (counter + 1) xs 
    in aux [] 0 lst
;;

(* find (fun a -> a > 5) [1; 2; 3; 4; 8; 9] = 8 *)
let rec find f lst = 
    match lst with
    | [] -> None
    | x::xs when (f x) -> Some x
    | _::xs -> find f xs
;;

(* Sieve of Eratosthenes, implemented imperatively, more natural this way *)
let sieve ubound =
    let primes = Array.init ubound (fun _ -> true) in
    let counter = ref 2 in
    while !counter < ubound do
        let p = ref (2 * !counter) in
        while !p <= ubound do
            primes.(!p - 1) <- false;
            p := !p + !counter;
        done;
        let candidates = (map (fun x -> x + 1) (index (fun b -> b) (Array.to_list primes))) in
        let temp = find (fun x -> x > !counter) candidates in
        counter := match temp with | None -> ubound | Some x -> x
    done;
    map (fun x -> x + 1) (index (fun b -> b) (Array.to_list primes))
;;

let is_prime num =
    match rev (sieve num) with
    | x::_ when x = num -> true
    | _ -> false
;;

(* Helper functions *)

let sort lst =
    sort (fun x y -> x<y) lst
;;

let is_subset sublst lst = 
    let rec aux sublst lst =
        match sublst, lst with
        | [], _ -> true
        | x::_, [] -> false
        | x::xs, y::ys when x < y -> false
        | x::xs, y::ys when x > y -> aux (x::xs) ys
        | x::xs, y::ys -> aux xs ys
    in aux (sort sublst) (sort lst)
;;

let inc x = (x + 1);;
let dec x = (x - 1);;

