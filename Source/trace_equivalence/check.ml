let suites = ref []
let add_suite s = suites := s :: !suites
let run () = ()(* Alcotest.run "Tests" !suites *)
