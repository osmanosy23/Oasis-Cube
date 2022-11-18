open OUnit2

(* open Graphics *)
open Cube
open Actions

type color = White | Red | Blue | Orange | Yellow | Green

let white_face = [| White; White; White; White; White; White; White; White; White |]
let red_face = [| Red; Red; Red; Red; Red; Red; Red; Red; Red |]
let blue_face = [| Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue |]
let orange_face = [| Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange |]
let yellow_face = [| Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow |]
let green_face = [| Green; Green; Green; Green; Green; Green; Green; Green; Green |]
let cube = [| red_face; yellow_face; blue_face; white_face; orange_face; green_face |]

let turn_tests (name : string) input expected_output : test =
  name >:: fun _ -> assert_equal expected_output (Array.iter (fun _ -> ()) input)

let u'_red_face = [| Green; Green; Green; Red; Red; Red; Red; Red; Red |]
let u'_u'_red_face = [| Orange; Orange; Orange; Red; Red; Red; Red; Red; Red |]
let u'_green_face = [| Orange; Orange; Orange; Green; Green; Green; Green; Green; Green |]
let u'_u'_green_face = [| Blue; Blue; Blue; Green; Green; Green; Green; Green; Green |]
let u'_orange_face = [| Blue; Blue; Blue; Orange; Orange; Orange; Orange; Orange; Orange |]
let u'_u'_orange_face = [| Red; Red; Red; Orange; Orange; Orange; Orange; Orange; Orange |]
let u'_blue_face = [| Red; Red; Red; Blue; Blue; Blue; Blue; Blue; Blue |]
let u'_u'_blue_face = [| Green; Green; Green; Blue; Blue; Blue; Blue; Blue; Blue |]
let u'_cube = [| u'_red_face; yellow_face; u'_blue_face; white_face; u'_orange_face; u'_green_face |]
let u_u_cube = [| u'_u'_red_face; yellow_face; u'_u'_blue_face; white_face; u'_u'_orange_face; u'_u'_green_face |]

let u'_tests =
  [
    turn_tests "testing one u' turn on the default cube" u'_cube (u'_turn cube);
    turn_tests
      "testing two u' turns on the default cube (i.e., one u' turn from\n\
      \  the cube in the state directly after one u' turn)" u_u_cube (u'_turn u'_cube);
    turn_tests
      "one u turn on the cube that has made a u' turn should be equivalent default cube (i.e., the state that hasn't \
       changed)"
      cube (u_turn u'_cube);
  ]

let u_red_face = [| Blue; Blue; Blue; Red; Red; Red; Red; Red; Red |]
let u_u_red_face = [| Orange; Orange; Orange; Red; Red; Red; Red; Red; Red |]
let u_green_face = [| Red; Red; Red; Green; Green; Green; Green; Green; Green |]
let u_u_green_face = [| Blue; Blue; Blue; Green; Green; Green; Green; Green; Green |]
let u_orange_face = [| Green; Green; Green; Orange; Orange; Orange; Orange; Orange; Orange |]
let u_u_orange_face = [| Red; Red; Red; Orange; Orange; Orange; Orange; Orange; Orange |]
let u_blue_face = [| Orange; Orange; Orange; Blue; Blue; Blue; Blue; Blue; Blue |]
let u_u_blue_face = [| Green; Green; Green; Blue; Blue; Blue; Blue; Blue; Blue |]
let u_cube = [| u_red_face; yellow_face; u_blue_face; white_face; u_orange_face; u_green_face |]
let u_u_cube = [| u_u_red_face; yellow_face; u_u_blue_face; white_face; u_u_orange_face; u_u_green_face |]

let u_tests =
  [
    turn_tests "testing one u turn on the default cube" u_cube (u_turn cube);
    turn_tests "testing two u turns on the default cube" u_u_cube (u_turn u_cube);
    turn_tests
      "one u' turn on the cube that has made a u turn should be equivalent default cube (i.e., the state that hasn't \
       changed)"
      cube (u'_turn u_cube);
  ]

let suite = "test suite for Cube" >::: List.flatten [ u'_tests; u_tests ]
let _ = run_test_tt_main suite