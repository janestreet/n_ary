open! Base
open Expect_test_helpers_base

let%expect_test _ =
  let exit_code = Stdlib.Sys.command "../gen/gen_n_ary.exe -help" in
  [%expect
    {|
    Usage:

      gen_n_ary.exe FILENAME

    Here, FILENAME is one of:

      jbuild
      n_ary.ml
      enum${n}.{ml,mli}    # for n={2..16}
      tuple${n}.{ml,mli}   # for n={2..16}
      variant${n}.{ml,mli} # for n={2..16}
      list${n}.{ml,mli}    # for n={2..16} |}];
  require_equal [%here] (module Int) exit_code 0;
  [%expect {| |}]
;;
