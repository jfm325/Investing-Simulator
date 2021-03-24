open OUnit2

let dummy_test name = name >:: fun _ -> assert true

let suite = "test suite" >::: [ dummy_test "1" ]

let _ = run_test_tt_main suite
