open Solutions

let () =
    print_string "Testing sort... ";
    assert(sort [] = []);
    assert(sort [-1] = [-1]);
    assert(sort [1;2;3] = [1;2;3]);
    assert(sort [6.99; 3.44; (-8.88); (-1.00); 0.00] = [(-8.88); (-1.00); 0.00; 3.44; 6.99]);
    assert(sort ["r"; "w"; "a"; "e"; "p"] = ["a"; "e"; "p"; "r"; "w"]);
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

    print_string "Testing length_histogram... ";
    assert(length_histogram [[1; 2; 3;]; []; [1; 2]; [1; 2]; [1]; [1]; [1]] =
        [(1, 3); (0, 1); (3, 1); (2, 2)]);
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

    print_string "1st problem... ";
    assert(last [1; 2; 3] = Some 3);
    assert(last [] = None);
    assert(last ["a"; "b"; "c"] = Some "c");
    print_string "PASSED\n";

    print_string "2nd problem... ";
    assert(last_two [1; 2; 3] = Some (2,3));
    assert(last_two [] = None);
    assert(last_two ["a"; "b"; "c"] = Some ("b","c"));
    assert(last_two ["a"] = None);
    print_string "PASSED\n";

    print_string "3rd problem... ";
    assert(at 0 ["a"] = Some "a");
    assert(at 1 [1; 2; 3] = Some 2);
    assert(at 1 [] = None);
    assert(at 2 [0; 1; 2; 3; 4] = Some 2);
    assert(at 3 ["a"; "b"; "c"] = None);
    print_string "PASSED\n";

    print_string "4th problem... ";
    assert(length ["a"] = 1);
    assert(length [1; 2; 3] = 3);
    assert(length [] = 0);
    assert(length [0; 1; 2; 3; 4] = 5);
    assert(length ["a"; "b"; "c"] = 3);
    print_string "PASSED\n";

    print_string "5th problem... ";
    assert(rev ["a"] = ["a"]);
    assert(rev [1; 2; 3] = [3; 2; 1]);
    assert(rev [] = []);
    assert(rev ["a"; "b"; "c"] = ["c"; "b"; "a"]);
    print_string "PASSED\n";

    print_string "6th problem... ";
    assert(is_palindrome ["a"]);
    assert(is_palindrome [1; 2; 1]);
    assert(is_palindrome []);
    assert(is_palindrome ["b"; "o"; "o"; "b"]);
    assert(not (is_palindrome [1; 2; 3]));
    assert(not (is_palindrome [1; 2]));
    print_string "PASSED\n";

    print_string "7th problem... ";
    assert(flatten [] = []);
    assert(flatten [ManyNode [ManyNode []]] = []);
    assert(flatten [ OneNode `a ; ManyNode [ OneNode `b ; ManyNode [ OneNode `c
    ; OneNode `d ] ; OneNode `e ] ] = [ `a ; `b ; `c ; `d ; `e ]);
    assert(flatten [ OneNode `a ; OneNode `b ; OneNode `c ; OneNode `d ;
    OneNode `e ] = [ `a ; `b ; `c ; `d ; `e ]);
    print_string "PASSED\n";

    print_string "8th problem... ";
    assert(compress [] = []);
    assert(compress [`a;`a;`a;`a;`b;`c;`c;`a;`a;`d;`e;`e;`e;`e] = [`a;`b;`c;`a;`d;`e]);
    print_string "PASSED\n";

    print_string "9th problem... ";
    assert(pack [] = []);
    assert(pack [`a] = [[`a]]);
    assert(pack [`a;`a;`a;`a;`b;`c;`c;`a;`a;`d;`d;`e;`e;`e;`e] = [[`a;`a;`a;`a]; [`b]; [`c;`c]; [`a;`a]; [`d;`d]; [`e;`e;`e;`e]]);
    assert(pack [1;2;2;3;3;3;4;4;4;4] = [[1];[2;2];[3;3;3];[4;4;4;4]]);
    assert(pack [1;2;2;3;3;3;4;4;4;4;1] = [[1];[2;2];[3;3;3];[4;4;4;4];[1]]);
    print_string "PASSED\n";

    print_string "10th problem... ";
    assert(encode [] = []);
    assert(encode [`a;`a;`a;`a;`b;`c;`c;`a;`a;`d;`e;`e;`e;`e] = [4,`a ; 1,`b ; 2,`c ; 2,`a ; 1,`d ; 4,`e]);
    assert(encode [1;1;1;2;2;2;2;3;3;4] = [3,1 ; 4,2; 2,3; 1,4]);
    assert(encode ["a";"b";"b";"c"] = [1,"a"; 2,"b"; 1,"c"]);
    assert(encode ["a"] = [1,"a"]);
    print_string "PASSED\n";

    print_string "11th problem... ";
    assert(encode2 [] = []);
    assert(encode2 [`a;`a;`a;`a;`b;`c;`c;`a;`a;`d;`e;`e;`e;`e] = [ManyRLE (4,`a) ;
    OneRLE `b ; ManyRLE (2,`c) ; ManyRLE (2,`a) ; OneRLE `d ; ManyRLE (4,`e)]);
    assert(encode2 [1;1;1;2;2;2;2;3;3;4] = [ManyRLE (3,1) ; ManyRLE (4,2);
    ManyRLE (2,3); OneRLE 4]);
    assert(encode2 ["a";"b";"b";"c"] = [OneRLE "a"; ManyRLE (2,"b"); OneRLE ("c")]);
    assert(encode2 ["a"] = [OneRLE "a"]);
    print_string "PASSED\n";

    print_string "12th problem... ";
    assert(decode [] = []);
    assert(decode [ManyRLE (4,`a) ; OneRLE `b ; ManyRLE (2,`c) ; ManyRLE (2,`a) ; OneRLE `d ; ManyRLE (4,`e)] = [`a;`a;`a;`a;`b;`c;`c;`a;`a;`d;`e;`e;`e;`e]);
    assert(decode [ManyRLE (3,1) ; ManyRLE (4,2); ManyRLE (2,3); OneRLE 4] = [1;1;1;2;2;2;2;3;3;4]);
    assert(decode [OneRLE "a"; ManyRLE (2,"b"); OneRLE ("c")] = ["a";"b";"b";"c"]);
    assert(decode [OneRLE "a"] = ["a"]);
    print_string "PASSED\n";

    print_string "13th problem... ";
    assert(encode3 [] = []);
    assert(encode3 [`a;`a;`a;`a;`b;`c;`c;`a;`a;`d;`e;`e;`e;`e] = [ManyRLE (4,`a) ;
    OneRLE `b ; ManyRLE (2,`c) ; ManyRLE (2,`a) ; OneRLE `d ; ManyRLE (4,`e)]);
    assert(encode3 [1;1;1;2;2;2;2;3;3;4] = [ManyRLE (3,1) ; ManyRLE (4,2);
    ManyRLE (2,3); OneRLE 4]);
    assert(encode3 ["a";"b";"b";"c"] = [OneRLE "a"; ManyRLE (2,"b"); OneRLE ("c")]);
    assert(encode3 ["a"] = [OneRLE "a"]);
    print_string "PASSED\n";

    print_string "14th problem... ";
    assert(duplicate [] = []);
    assert(duplicate [`a;`b;`c;`c;`d] = [`a;`a;`b;`b;`c;`c;`c;`c;`d;`d]);
    assert(duplicate [1] = [1; 1]);
    print_string "PASSED\n";

    print_string "15th problem... ";
    assert(replicate [] 3 = []);
    assert(replicate [`a;`b;`c] 3 = [`a;`a;`a;`b;`b;`b;`c;`c;`c]);
    assert(replicate [1] 3 = [1; 1; 1]);
    print_string "PASSED\n";

    print_string "16th problem... ";
    assert(drop [`a;`b;`c;`d;`e;`f;`g;`h;`i;`j] 3 = [`a;`b;`d;`e;`g;`h;`j]);
    assert(drop [1; 2; 3] 5 = [1; 2; 3]);
    assert(drop [1; 2; 3; 4; 5; 6] 2 = [1; 3; 5]);
    assert(drop [1; 2; 3] 1 = []);
    print_string "PASSED\n";

    print_string "17th problem... ";
    assert(split [`a;`b;`c;`d;`e;`f;`g;`h;`i;`j] 3 = ([`a;`b;`c] , [`d;`e;`f;`g;`h;`i;`j]));
    assert(split [`a;`b;`c;`d] 5 = ([`a; `b; `c; `d], []));
    print_string "PASSED\n";

    print_string "18th problem... ";
    assert(slice [`a;`b;`c;`d;`e;`f;`g;`h;`i;`j] 2 6 = [`c;`d;`e;`f;`g]);
    assert(slice [1; 2; 3; 4; 5] 2 10 = [3; 4; 5]);
    assert(slice [1; 2; 3; 4; 5] 2 2 = [3]);
    print_string "PASSED\n";

    print_string "19th problem... ";
    assert(rotate [`a;`b;`c;`d;`e;`f;`g;`h] 3 = [`d;`e;`f;`g;`h;`a;`b;`c]);
    assert(rotate [`a;`b;`c;`d;`e;`f;`g;`h] 11 = [`d;`e;`f;`g;`h;`a;`b;`c]);
    assert(rotate [`a;`b;`c;`d;`e;`f;`g;`h] (-10) = [`g;`h;`a;`b;`c;`d;`e;`f]);
    assert(rotate [`a;`b;`c;`d;`e;`f;`g;`h] (-2) = [`g;`h;`a;`b;`c;`d;`e;`f]);
    print_string "PASSED\n";

    print_string "20th problem... ";
    assert(remove_at 0 [`a;`b;`c;`d] = [`b;`c;`d]);
    assert(remove_at 0 [] = []);
    assert(remove_at 1 [`a;`b;`c;`d] = [`a;`c;`d]);
    assert(remove_at 3 [1; 2; 3; 4] = [1; 2; 3]);
    assert(remove_at 4 [1; 2; 3; 4] = [1; 2; 3; 4]);
    print_string "PASSED\n";

    print_string "21st problem... ";
    assert(insert_at `alfa 1 [`a;`b;`c;`d] = [`a;`alfa;`b;`c;`d]);
    assert(insert_at `a 0 [] = [`a]);
    assert(insert_at `a 0 [`b] = [`a; `b]);
    assert(insert_at `a 1 [`b] = [`b; `a]);
    assert(insert_at `c 2 [`a; `b] = [`a; `b; `c]);
    print_string "PASSED\n";

    print_string "22nd problem... ";
    assert(range 4 9 = [4;5;6;7;8;9]);
    assert(range 9 4 = [9;8;7;6;5;4]);
    assert(range 5 5 = [5]);
    print_string "PASSED\n";

    print_string "23rd problem... ";
    assert(length (rand_select [`a; `b; `c; `d; `e; `f; `g] 2)=2);
    assert(length (rand_select [`a; `b; `c; `d; `e; `f; `g] 3)=3);
    assert(is_subset (rand_select [3; 2; 4; 1; 5] 2) [3; 2; 4; 1; 5]);
    assert(is_subset (rand_select ["a"; "g"; "r"; "y"; "d"] 2) ["a"; "g"; "r"; "y"; "d"]);
    print_string "PASSED\n";

    print_string "24th problem... ";
    assert(length (lotto_select 3 8)=3);
    assert(length (lotto_select 50 100)=50);
    assert(is_subset (lotto_select 20 100) (range 1 100));
    assert(is_subset (lotto_select 10 20) (range 1 20));
    print_string "PASSED\n";

    print_string "25th problem... ";
    assert(length (permutation [1; 2; 3; 4; 5]) = (length [1; 2; 3; 4; 5]));
    assert(sort (permutation [4; 3; 5; 9; (-1); 3; 3; (-5); 0]) = (sort [4; 3; 5; 9; (-1); 3; 3; (-5); 0]));
    assert(sort (permutation ["er"; "tj"; "df"; "df"; "dfdf"; "df"]) = (sort ["er"; "tj"; "df"; "df"; "dfdf"; "df"]));
    print_string "PASSED\n";

    print_string "26th problem... ";
    assert(extract 1 ["a"] = [["a"]]);
    assert(map sort (extract 2 ["a"; "b"; "c"]) = [["a"; "b"]; ["a"; "c"]; ["b"; "c"]]);
    assert(map sort (extract 3 [1;2;3;4;5]) =
        [[1;2;3];[1;2;4];[1;2;5];[1;3;4];[1;3;5];[1;4;5];[2;3;4];[2;3;5];[2;4;5];[3;4;5]]);
    print_string "PASSED\n";

    print_string "27th problem... ";
    assert(group ["a"; "b"; "c"] [2;1] = [[["c"]; ["b"; "a"]]; [["b"]; ["c"; "a"]]; [["a"]; ["c"; "b"]]]);
    assert(group ["a"; "b"; "c"] [1;2] = [[["c"; "b"]; ["a"]]; [["c"; "a"]; ["b"]]; [["b"; "a"]; ["c"]]]);
    assert(group [`a;`b;`c;`d] [2;1] =
        [[[`c]; [`b; `a]]; [[`d]; [`b; `a]]; [[`b]; [`c; `a]]; [[`d]; [`c; `a]];
         [[`b]; [`d; `a]]; [[`c]; [`d; `a]]; [[`a]; [`c; `b]]; [[`d]; [`c; `b]];
         [[`a]; [`d; `b]]; [[`c]; [`d; `b]]; [[`a]; [`d; `c]]; [[`b]; [`d; `c]]]);
    print_string "PASSED\n";

    print_string "28th problem... ";
    assert(length_sort [[`a;`b;`c]; [`d;`e]; [`f;`g;`h]; [`d;`e];
                        [`i;`j;`k;`l]; [`m;`n]; [`o]] =
                    [[`o]; [`m; `n]; [`d; `e]; [`d; `e]; [`f; `g; `h]; [`a; `b;
                    `c]; [`i; `j; `k; `l]]);
    assert(frequency_sort [ [`a;`b;`c]; [`d;`e]; [`f;`g;`h]; [`d;`e];
                   [`i;`j;`k;`l]; [`m;`n]; [`o] ] =
               [[`o]; [`i; `j; `k; `l]; [`f; `g; `h]; [`a; `b; `c]; [`m; `n];
               [`d; `e]; [`d; `e]]);
    print_string "PASSED\n";

    print_string "29th problem... ";
    assert(is_prime(1)  = true);
    assert(is_prime(2)  = true);
    assert(is_prime(97)  = true);
    assert(is_prime(101)  = true);
    assert(is_prime(113)  = true);
    assert(is_prime(4)  = false);
    assert(is_prime(9)  = false);
    assert(is_prime(50)  = false);
    assert(is_prime(55)  = false);
    print_string "PASSED\n";

;;

