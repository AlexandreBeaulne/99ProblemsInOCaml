open Core.Std
open Solutions

let () =
    print_string "1st problem... ";
    assert (last [1; 2; 3] = Some 3);
    assert (last [] = None);
    assert (last ["a"; "b"; "c"] = Some "c");
    print_string "PASSED\n";

    print_string "2nd problem... ";
    assert (last_two [1; 2; 3] = Some (2,3));
    assert (last_two [] = None);
    assert (last_two ["a"; "b"; "c"] = Some ("b","c"));
    assert (last_two ["a"] = None);
    print_string "PASSED\n";

    print_string "3rd problem... ";
    assert (at 0 ["a"] = Some "a");
    assert (at 1 [1; 2; 3] = Some 2);
    assert (at 1 [] = None);
    assert (at 2 [0; 1; 2; 3; 4] = Some 2);
    assert (at 3 ["a"; "b"; "c"] = None);
    print_string "PASSED\n";

    print_string "4th problem... ";
    assert (length ["a"] = 1);
    assert (length [1; 2; 3] = 3);
    assert (length [] = 0);
    assert (length [0; 1; 2; 3; 4] = 5);
    assert (length ["a"; "b"; "c"] = 3);
    print_string "PASSED\n";

    print_string "5th problem... ";
    assert (rev ["a"] = ["a"]);
    assert (rev [1; 2; 3] = [3; 2; 1]);
    assert (rev [] = []);
    assert (rev ["a"; "b"; "c"] = ["c"; "b"; "a"]);
    print_string "PASSED\n";

    print_string "6th problem... ";
    assert (is_palindrome ["a"]);
    assert (is_palindrome [1; 2; 1]);
    assert (is_palindrome []);
    assert (is_palindrome ["b"; "o"; "o"; "b"]);
    assert (not (is_palindrome [1; 2; 3]));
    assert (not (is_palindrome [1; 2]));
    print_string "PASSED\n";

    print_string "7th problem... ";
    assert (flatten [] = []);
    assert (flatten [ManyNode [ManyNode []]] = []);
    assert (flatten [ OneNode `a ; ManyNode [ OneNode `b ; ManyNode [ OneNode `c
    ; OneNode `d ] ; OneNode `e ] ] = [ `a ; `b ; `c ; `d ; `e ]);
    assert (flatten [ OneNode `a ; OneNode `b ; OneNode `c ; OneNode `d ;
    OneNode `e ] = [ `a ; `b ; `c ; `d ; `e ]);
    print_string "PASSED\n";

    print_string "8th problem... ";
    assert (compress [] = []);
    assert (compress [`a;`a;`a;`a;`b;`c;`c;`a;`a;`d;`e;`e;`e;`e] = [`a;`b;`c;`a;`d;`e]);
    print_string "PASSED\n";

    print_string "9th problem... ";
    assert (pack [] = []);
    assert (pack [`a] = [[`a]]);
    assert (pack [`a;`a;`a;`a;`b;`c;`c;`a;`a;`d;`d;`e;`e;`e;`e] = [[`a;`a;`a;`a]; [`b]; [`c;`c]; [`a;`a]; [`d;`d]; [`e;`e;`e;`e]]);
    assert (pack [1;2;2;3;3;3;4;4;4;4] = [[1];[2;2];[3;3;3];[4;4;4;4]]);
    assert (pack [1;2;2;3;3;3;4;4;4;4;1] = [[1];[2;2];[3;3;3];[4;4;4;4];[1]]);
    print_string "PASSED\n";

    print_string "10th problem... ";
    assert (encode [] = []);
    assert (encode [`a;`a;`a;`a;`b;`c;`c;`a;`a;`d;`e;`e;`e;`e] = [4,`a ; 1,`b ; 2,`c ; 2,`a ; 1,`d ; 4,`e]);
    assert (encode [1;1;1;2;2;2;2;3;3;4] = [3,1 ; 4,2; 2,3; 1,4]);
    assert (encode ["a";"b";"b";"c"] = [1,"a"; 2,"b"; 1,"c"]);
    assert (encode ["a"] = [1,"a"]);
    print_string "PASSED\n";

    print_string "11th problem... ";
    assert (encode2 [] = []);
    assert (encode2 [`a;`a;`a;`a;`b;`c;`c;`a;`a;`d;`e;`e;`e;`e] = [ManyRLE (4,`a) ;
    OneRLE `b ; ManyRLE (2,`c) ; ManyRLE (2,`a) ; OneRLE `d ; ManyRLE (4,`e)]);
    assert (encode2 [1;1;1;2;2;2;2;3;3;4] = [ManyRLE (3,1) ; ManyRLE (4,2);
    ManyRLE (2,3); OneRLE 4]);
    assert (encode2 ["a";"b";"b";"c"] = [OneRLE "a"; ManyRLE (2,"b"); OneRLE ("c")]);
    assert (encode2 ["a"] = [OneRLE "a"]);
    print_string "PASSED\n";

    print_string "12th problem... ";
    assert (decode [] = []);
    assert (decode [ManyRLE (4,`a) ; OneRLE `b ; ManyRLE (2,`c) ; ManyRLE (2,`a) ; OneRLE `d ; ManyRLE (4,`e)] = [`a;`a;`a;`a;`b;`c;`c;`a;`a;`d;`e;`e;`e;`e]);
    assert (decode [ManyRLE (3,1) ; ManyRLE (4,2); ManyRLE (2,3); OneRLE 4] = [1;1;1;2;2;2;2;3;3;4]);
    assert (decode [OneRLE "a"; ManyRLE (2,"b"); OneRLE ("c")] = ["a";"b";"b";"c"]);
    assert (decode [OneRLE "a"] = ["a"]);
    print_string "PASSED\n";

    print_string "13th problem... ";
    assert (encode3 [] = []);
    assert (encode3 [`a;`a;`a;`a;`b;`c;`c;`a;`a;`d;`e;`e;`e;`e] = [ManyRLE (4,`a) ;
    OneRLE `b ; ManyRLE (2,`c) ; ManyRLE (2,`a) ; OneRLE `d ; ManyRLE (4,`e)]);
    assert (encode3 [1;1;1;2;2;2;2;3;3;4] = [ManyRLE (3,1) ; ManyRLE (4,2);
    ManyRLE (2,3); OneRLE 4]);
    assert (encode3 ["a";"b";"b";"c"] = [OneRLE "a"; ManyRLE (2,"b"); OneRLE ("c")]);
    assert (encode3 ["a"] = [OneRLE "a"]);
    print_string "PASSED\n";

    print_string "14th problem... ";
    assert (duplicate [] = []);
    assert (duplicate [`a;`b;`c;`c;`d] = [`a;`a;`b;`b;`c;`c;`c;`c;`d;`d]);
    assert (duplicate [1] = [1; 1]);
    print_string "PASSED\n";

    print_string "15th problem... ";
    assert (replicate [] 3 = []);
    assert (replicate [`a;`b;`c] 3 = [`a;`a;`a;`b;`b;`b;`c;`c;`c]);
    assert (replicate [1] 3 = [1; 1; 1]);
    print_string "PASSED\n";

    print_string "16th problem... ";
    assert (drop [`a;`b;`c;`d;`e;`f;`g;`h;`i;`j] 3 = [`a;`b;`d;`e;`g;`h;`j]);
    assert (drop [1; 2; 3] 5 = [1; 2; 3]);
    assert (drop [1; 2; 3; 4; 5; 6] 2 = [1; 3; 5]);
    assert (drop [1; 2; 3] 1 = []);
    print_string "PASSED\n";

    print_string "17th problem... ";
    assert (split [`a;`b;`c;`d;`e;`f;`g;`h;`i;`j] 3 = ([`a;`b;`c] , [`d;`e;`f;`g;`h;`i;`j]));
    assert (split [`a;`b;`c;`d] 5 = ([`a; `b; `c; `d], []));
    print_string "PASSED\n";

    print_string "18th problem... ";
    assert (slice [`a;`b;`c;`d;`e;`f;`g;`h;`i;`j] 2 6 = [`c;`d;`e;`f;`g]);
    assert (slice [1; 2; 3; 4; 5] 2 10 = [3; 4; 5]);
    assert (slice [1; 2; 3; 4; 5] 2 2 = [3]);
    print_string "PASSED\n";

    print_string "19th problem... ";
    assert(rotate [`a;`b;`c;`d;`e;`f;`g;`h] 3 = [`d;`e;`f;`g;`h;`a;`b;`c]);
    assert(rotate [`a;`b;`c;`d;`e;`f;`g;`h] (-2) = [`g;`h;`a;`b;`c;`d;`e;`f]);
    print_string "PASSED\n";

;;

