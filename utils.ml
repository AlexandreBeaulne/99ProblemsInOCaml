
(* Reinvent the wheel on various oft-used functions *)

(* List *)
let length lst = 
    let rec aux acc lst =
        match lst with
        | [] -> acc
        | x::xs -> aux (acc+1) xs
    in aux 0 lst
;;

let rev lst =
    let rec aux acc = function
        | [] -> acc
        | x::xs -> aux (x::acc) xs
    in aux [] lst
;;

let split lst num =
    let rec aux acc counter lst =
        match counter, lst with
        | _, [] -> ((rev acc), [])
        | 0, lst -> ((rev acc), lst)
        | counter, x::xs -> aux (x::acc) (counter-1) xs
    in aux [] num lst
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

(* Sort *)

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

(* Set *) 

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

let is_subset sublst lst = 
    let rec aux sublst lst =
        match sublst, lst with
        | [], _ -> true
        | x::_, [] -> false
        | x::xs, y::ys when x < y -> false
        | x::xs, y::ys when x > y -> aux (x::xs) ys
        | x::xs, y::ys -> aux xs ys
    in aux (sort (fun x y -> x < y) sublst) (sort (fun x y -> x < y) lst)
;;

(* KeyValue stores *)

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

(* map, filter, reduce, etc... *)

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

let filter f lst = 
    let rec aux acc lst =
        match lst with
        | [] -> rev acc
        | x::xs when (f x) -> aux (x::acc) xs
        | _::xs -> aux acc xs
    in aux [] lst
;;

let rec reduce f base lst =
    match lst with
    | [] -> base
    | x::xs -> reduce f (f base x) xs
;;

(* Math *)

let exp base power = 
    let rec aux acc power =
        if power = 0
        then acc
        else aux (acc*base) (power -1)
    in aux 1 power
;;

let inc x = (x + 1);;
let dec x = (x - 1);;

(* Other *)

let time action arg =
    (* in milliseconds *)
    let start_time = Unix.gettimeofday() in
    ignore (action arg);
    let finish_time = Unix.gettimeofday() in
    1000.0 *. (finish_time -. start_time)
;;

