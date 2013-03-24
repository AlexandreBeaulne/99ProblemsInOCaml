
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

