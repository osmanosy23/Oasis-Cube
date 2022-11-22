open OUnit2

(* open Graphics *)
open Cube
open Actions

type color = White | Red | Blue | Orange | Yellow | Green
 let white_face = [| White; White; White; White; White; White; White; White; White |]
 (* let red_face = [| Red; Red; Red; Red; Red; Red; Red; Red; Red |]   *)
 let yellow_face = [| Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow |]
 (* let blue_face = [| Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue |]  *)
 (* let orange_face = [| Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange |]   *)
 (* let green_face = [| Green; Green; Green; Green; Green; Green; Green; Green; Green |]   *)
 (* let base_cube = [| red_face; yellow_face; blue_face; white_face; orange_face; green_face |]  *)
let create_cube = 
  let white_f = [| White; White; White; White; White; White; White; White; White |] in
  let red_f = [| Red; Red; Red; Red; Red; Red; Red; Red; Red |] in
  let blue_f = [| Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue |] in
  let orange_f = [| Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange |] in
  let yellow_f = [| Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow |] in 
  let green_f = [| Green; Green; Green; Green; Green; Green; Green; Green; Green |] in
  [| red_f; yellow_f; blue_f; white_f; orange_f; green_f |]

let cts = function
  White -> "White "
  | Yellow -> "Yellow "
  | Red -> "Red "
  | Orange -> "Orange "
  | Blue -> "Blue "
  | Green -> "Green "

let face_to_string = function 
  [|a;b;c;d;e;f;g;h;i|] -> cts a ^ cts b ^ cts c ^ cts d ^ cts e ^ cts f ^ cts g ^ cts h ^ cts i
  | _ -> failwith "invalid"

let cube_to_string = function 
  [|a;b;c;d;e;f|] -> "Face 1: " ^ face_to_string a ^"Face 2: " ^ face_to_string b ^"Face 3: " ^ 
  face_to_string c ^"Face 4: " ^ face_to_string d ^"Face 5: " ^ face_to_string e ^"Face 6: " ^ face_to_string f 
  | _ -> failwith "invalid"

let face_copy = function 
  [|a;b;c;d;e;f;g;h;i|] -> [|a;b;c;d;e;f;g;h;i|]
  | _ -> failwith "invalid"

let cube_copy = function 
  [|a;b;c;d;e;f|] -> [|face_copy a;face_copy b;face_copy c;face_copy d;face_copy e;face_copy f;|]
  | _ -> failwith "invalid"

let init_cube turn random= 
  let return_cube = cube_copy create_cube in
  randomize return_cube random; 
  turn return_cube;
  return_cube 

let init_cube_two_turns turn1 turn2 random= 
  let return_cube = cube_copy create_cube in
  randomize return_cube random; 
  turn1 return_cube;
  turn2 return_cube; 
  return_cube 

let check_undo turn1 turn2 random = 
    let return_cube = cube_copy create_cube in
    randomize return_cube random;
    let cube_dupe =  (return_cube) |> cube_copy in
    turn1 return_cube;
    turn2 return_cube;
    print_string ("cube_dupe = "^ cube_to_string cube_dupe);
    print_string ("return cube = " ^ cube_to_string return_cube);
    return_cube  = cube_dupe

let turn_tests (name : string) input1 input2  : test =
  name >:: fun _ -> assert_equal input1 input2

let u'_red_face = [| Red; Red; Red; Red; Red; Red; Blue; Blue; Blue |]
let u'_u'_red_face = [| Red; Red; Red; Red; Red; Red; Orange; Orange; Orange |]
let u'_green_face = [| Green; Green; Green; Green; Green; Green; Red; Red; Red |]
let u'_u'_green_face = [| Green; Green; Green; Green; Green; Green; Blue; Blue; Blue |]
let u'_orange_face = [| Orange; Orange; Orange; Orange; Orange; Orange; Green; Green; Green |]
let u'_u'_orange_face = [| Orange; Orange; Orange; Orange; Orange; Orange; Red; Red; Red |]
let u'_blue_face = [| Blue; Blue; Blue; Blue; Blue; Blue; Orange; Orange; Orange |]
let u'_u'_blue_face = [| Blue; Blue; Blue; Blue; Blue; Blue; Green; Green; Green |]
let u'_cube = [| u'_red_face; yellow_face; u'_blue_face; white_face; u'_orange_face; u'_green_face |] 
let u'_u'_cube = [| u'_u'_red_face; yellow_face; u'_u'_blue_face; white_face; u'_u'_orange_face; u'_u'_green_face |] 

let u'_tests =
  [
    turn_tests "default cube; testing one u' turn on the default cube" u'_cube (init_cube u'_turn 0); 
    turn_tests
      "testing two u' turns on the default cube (i.e., one u' turn from\n\
      \  the cube in the state directly after one u' turn)"  u'_u'_cube (init_cube_two_turns u'_turn u'_turn 0);
    turn_tests
      "one u turn on the cube that has made a u' turn should be equivalent default cube (i.e., the state that hasn't \
       changed)"
       true (check_undo u'_turn u_turn 100); 
  ]

(* let u_red_face = [| Blue; Blue; Blue; Red; Red; Red; Red; Red; Red |]
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
  ] *)

let suite = "test suite for Cube" >::: List.flatten [ u'_tests; ]
let _ = run_test_tt_main suite