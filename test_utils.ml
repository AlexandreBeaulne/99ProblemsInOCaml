
open Utils

let () =
    print_string "Testing sort... ";
    assert(sort (fun x y -> x < y) [] = []);
    assert(sort (fun x y -> x < y) [-1] = [-1]);
    assert(sort (fun x y -> x < y) [1;2;3] = [1;2;3]);
    assert(sort (fun x y -> x < y) [6.99; 3.44; (-8.88); (-1.00); 0.00] = [(-8.88); (-1.00); 0.00; 3.44; 6.99]);
    assert(sort (fun x y -> x < y) ["r"; "w"; "a"; "e"; "p"] = ["a"; "e"; "p"; "r"; "w"]);
    print_string "PASSED\n";

    print_string "Testing is_subset... ";
    assert(is_subset [2; 4; 6] [1; 2; 3; 4; 5; 6; 7; 8]);
    assert(is_subset [2; 4; 6] [8; 6; 7; 4; 5; 2; 3; 1]);
    assert(is_subset ["d"; "e"; "f"] ["a"; "b"; "c"; "d"; "e"; "f"]);
    assert(is_subset [] [`a; `b; `c]);
    assert(is_subset ["string1"] ["string1"; "string2"]);
    assert(not (is_subset [1; 2; 3] [1; 2]));
    assert(not (is_subset [1] []));
    print_string "PASSED\n";

    print_string "Testing inc... ";
    assert(inc 2 = 3);
    assert(inc (-3) = (-2));
    print_string "PASSED\n";

    print_string "Testing dec... ";
    assert(dec 2 = 1);
    assert(dec (-3) = (-4));
    print_string "PASSED\n";

    print_string "Testing map... ";
    assert(map inc [] = []);
    assert(map inc [1; 2; 3] = [2; 3; 4]);
    print_string "PASSED\n";

    print_string "Testing inside... ";
    assert(inside 3 [1; 2; 3; 4]);
    assert(not (inside 5 [1; 2; 3; 4]));
    assert(inside "c" ["c"; "b"; "a"; "d"]);
    assert(inside "c" ["d"; "b"; "a"; "c"]);
    assert(not (inside "e" ["d"; "b"; "a"; "c"]));
    print_string "PASSED\n";

    print_string "Testing complement... ";
    assert(complement [1; 2; 3; 4; 5; 6] [1; 2; 3] = [4; 5; 6]);
    assert(complement [1; 2; 3; 4; 5; 6] [4; 5; 6] = [1; 2; 3]);
    assert(complement ["a"; "b"; "c"] [] = ["a"; "b"; "c"]);
    assert(complement ["a"; "b"; "c"] ["a"; "b"; "c"] = []);
    print_string "PASSED\n";

    print_string "Testing unpack... ";
    assert(unpack [[1; 2]; []; [3; 4]] = [1; 2; 3; 4]);
    print_string "PASSED\n";

    print_string "Testing map2... ";
    assert(map2 (fun x y -> (x+y)) [1; 1; 1] [1; 1; 1] = [2; 2; 2]);
    assert(map2 (fun x y -> (x+y)) [1; 2; (-2)] [4; (-4); 5] = [5; (-2); 3]);
    print_string "PASSED\n";

    print_string "Testing key_inc... ";
    assert(key_inc "a" [("a", 1); ("b",2)] =  [("a", 2); ("b", 2)]);
    assert(key_inc "c" [("a", 1); ("b",2)] =  [("c", 1); ("b", 2); ("a", 1)]);
    assert(key_inc 3 [(2, 1); (3, (-3))] =  [(3, (-2)); (2, 1)]);
    print_string "PASSED\n";

    print_string "Testing key_get... ";
    assert(key_get "key" [("a", 2); ("key", (-666)); ("b", (-222))] = (-666));
    assert(key_get "key" [("a", 2); ("c", (-666)); ("b", (-222))] = 0);
    print_string "PASSED\n";

    print_string "Testing index... ";
    assert(index (fun b -> b) [true; false; false; true; false; true] = [0; 3; 5]);
    assert(index (fun b -> b) [false; false; false] = []);
    assert(index (fun b -> b) [true; true; true] = [0; 1; 2]);
    print_string "PASSED\n";
    
    print_string "Testing find... ";
    assert(find (fun x -> x > 5) [1; 2; 3; 4; 5; 9; 8; 7; 1; 2; 3] = Some 9);
    assert(find (fun x -> x > 5) [1; 2; 3; 4; 5; 3; 3; 3; 1; 2; 3] = None);
    print_string "PASSED\n";

    print_string "Testing memoize... ";
    let memo_exp = memoize exp in
    assert(exp 3 4 = memo_exp 3 4);
    assert(exp (-3) 4 = memo_exp (-3) 4);
    print_string "PASSED\n";
;;

