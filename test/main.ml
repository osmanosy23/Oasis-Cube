open OUnit2

(* open Graphics *)
open Cube
open Actions

type color = White | Red | Blue | Orange | Yellow | Green
let white_face = [| White; White; White; White; White; White; White; White; White |]
let red_face = [| Red; Red; Red; Red; Red; Red; Red; Red; Red |]  
let yellow_face = [| Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow |]
let blue_face = [| Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue |] 
let orange_face = [| Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange |]  
let green_face = [| Green; Green; Green; Green; Green; Green; Green; Green; Green |]  
let base_cube = [| orange_face; yellow_face; green_face; white_face; red_face; blue_face |] 

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
  let return_cube = cube_copy base_cube in
  randomize return_cube random; 
  turn return_cube;
  return_cube 

let init_cube_two_turns turn1 turn2 random= 
  let return_cube = cube_copy base_cube in
  randomize return_cube random; 
  turn1 return_cube;
  turn2 return_cube; 
  return_cube 

let check_undo turn1 turn2 random = 
    let return_cube = cube_copy base_cube in
    randomize return_cube random;
    let cube_dupe =  return_cube|> cube_copy in
    turn1 return_cube;
    turn2 return_cube;
    return_cube  = cube_dupe
    
let check_undo_four_turns turn random = 
  let return_cube = cube_copy base_cube in 
  randomize return_cube random;
  let cube_dupe = return_cube |> cube_copy in 
  turn return_cube;
  turn return_cube;
  turn return_cube;
  turn return_cube;
  return_cube = cube_dupe

let turn_test (name : string) input1 input2  : test =
  name >:: fun _ -> assert_equal input1 input2 ~printer:cube_to_string
let undo_test (name : string) input  : test =
  name >:: fun _ -> assert_equal true input 
let u'_red_face = [| Red; Red; Red; Red; Red; Red; Blue; Blue; Blue |]
let u'_u'_red_face = [| Red; Red; Red; Red; Red; Red; Orange; Orange; Orange |]
let u'_green_face = [| Green; Green; Green; Green; Green; Green; Red; Red; Red |]
let u'_u'_green_face = [| Green; Green; Green; Green; Green; Green; Blue; Blue; Blue |]
let u'_orange_face = [| Orange; Orange; Orange; Orange; Orange; Orange; Green; Green; Green |]
let u'_u'_orange_face = [| Orange; Orange; Orange; Orange; Orange; Orange; Red; Red; Red |]
let u'_blue_face = [| Blue; Blue; Blue; Blue; Blue; Blue; Orange; Orange; Orange |]
let u'_u'_blue_face = [| Blue; Blue; Blue; Blue; Blue; Blue; Green; Green; Green |]
let u'_cube = [| u'_orange_face; yellow_face; u'_green_face; white_face; u'_red_face; u'_blue_face |] 
let u'_u'_cube = [| u'_u'_orange_face; yellow_face; u'_u'_green_face; white_face; u'_u'_red_face; u'_u'_blue_face |] 

let u'_tests =
  [
    turn_test "default cube; testing one u' turn on the default cube" u'_cube (init_cube u'_turn 0); 
    turn_test
      "testing two u' turns on the default cube (i.e., one u' turn from\n\
      \  the cube in the state directly after one u' turn)"  u'_u'_cube (init_cube_two_turns u'_turn u'_turn 0);
      undo_test
      "one u turn on the cube that has made a u' turn should be equivalent default cube (i.e., the state that hasn't \
       changed)"
        (check_undo u'_turn u_turn 100);
      undo_test "four u' turns should return the cube to its original state"  (check_undo_four_turns u'_turn 100);  
  ]

let u_red_face = [| Red; Red; Red; Red; Red; Red; Green; Green; Green |]
let u_u_red_face = [| Red; Red; Red; Red; Red; Red; Orange; Orange; Orange |]
let u_green_face = [| Green; Green; Green; Green; Green; Green; Orange; Orange; Orange; |]
let u_u_green_face = [| Green; Green; Green; Green; Green; Green; Blue; Blue; Blue |]
let u_orange_face = [| Orange; Orange; Orange; Orange; Orange; Orange; Blue; Blue; Blue |]
let u_u_orange_face = [| Orange; Orange; Orange; Orange; Orange; Orange; Red; Red; Red |]
let u_blue_face = [| Blue; Blue; Blue; Blue; Blue; Blue; Red; Red; Red |]
let u_u_blue_face = [| Blue; Blue; Blue; Blue; Blue; Blue; Green; Green; Green |]
let u_cube = [| u_orange_face; yellow_face; u_green_face; white_face; u_red_face; u_blue_face |]
let u_u_cube = [| u_u_orange_face; yellow_face; u_u_green_face; white_face; u_u_red_face; u_u_blue_face |]

let u_tests =
  [
    turn_test "testing one u turn on the default cube" u_cube (init_cube u_turn 0);
    turn_test "testing two u turns on the default cube" u_u_cube (init_cube_two_turns u_turn u_turn 0);
    undo_test
      "one u' turn on the cube that has made a u turn should be equivalent default cube (i.e., the state that hasn't \
       changed)"
       (check_undo u_turn u'_turn 1000);
      undo_test "four u turns should return the cube to its original state"  (check_undo_four_turns u_turn 100);  
  ] 

  let f_yellow_face = [|Blue; Blue; Blue; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow |]
  let f_f_yellow_face = [| White; White; White; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow |]
  let f_green_face = [| Yellow; Green; Green; Yellow; Green; Green; Yellow; Green; Green; |]
  let f_f_green_face = [| Blue; Green; Green; Blue; Green; Green; Blue; Green; Green; |]
  let f_white_face = [| White; White; White; White; White; White; Green; Green; Green |]
  let f_f_white_face = [| White; White; White; White; White; White; Yellow; Yellow; Yellow |]
  let f_blue_face = [| Blue; Blue; White; Blue; Blue; White; Blue; Blue; White |]
  let f_f_blue_face = [| Blue; Blue; Green; Blue; Blue; Green; Blue; Blue; Green |]
  let f_cube = [| orange_face; f_yellow_face; f_green_face; f_white_face; red_face; f_blue_face |]
  let f_f_cube = [| orange_face; f_f_yellow_face; f_f_green_face; f_f_white_face; red_face; f_f_blue_face |]
 
  let f_tests =
    [
      turn_test "testing one f turn on the default cube" f_cube (init_cube f_turn 0);
      turn_test "testing two f turns on the default cube" f_f_cube (init_cube_two_turns f_turn f_turn 0);
      undo_test
        "one f' turn on the cube that has made a f turn should be equivalent default cube (i.e., the state that hasn't \
         changed)"
         (check_undo f_turn f'_turn 1000);
      undo_test "four f turns should return the cube to its original state"  (check_undo_four_turns f_turn 100);  
    ] 
  
  let f'_yellow_face = [|Green; Green; Green; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow |]
  let f'_f'_yellow_face = [| White; White; White; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow |]
  let f'_green_face = [| White; Green; Green; White; Green; Green; White; Green; Green; |]
  let f'_f'_green_face = [| Blue; Green; Green; Blue; Green; Green; Blue; Green; Green; |]
  let f'_white_face = [| White; White; White; White; White; White; Blue; Blue; Blue |]
  let f'_f'_white_face = [| White; White; White; White; White; White; Yellow; Yellow; Yellow |]
  let f'_blue_face = [| Blue; Blue; Yellow; Blue; Blue; Yellow; Blue; Blue; Yellow |]
  let f'_f'_blue_face = [| Blue; Blue; Green; Blue; Blue; Green; Blue; Blue; Green |]
  let f'_cube = [| orange_face; f'_yellow_face; f'_green_face; f'_white_face; red_face; f'_blue_face |]
  let f'_f'_cube = [| orange_face; f'_f'_yellow_face; f'_f'_green_face; f'_f'_white_face; red_face; f'_f'_blue_face |]
 
  let f'_tests =
    [
      turn_test "testing one f' turn on the default cube" f'_cube (init_cube f'_turn 0);
      turn_test "testing two f' turns on the default cube" f'_f'_cube (init_cube_two_turns f'_turn f'_turn 0);
      undo_test
        "one f turn on the cube that has made a f' turn should be equivalent default cube (i.e., the state that hasn't \
         changed)"
         (check_undo f'_turn f_turn 1000);
      undo_test "four f' turns should return the cube to its original state"  (check_undo_four_turns f'_turn 100);  
    ] 
let _white_face = [| White; White; White; White; White; White; White; White; White |]
let __white_face = [| White; White; White; White; White; White; White; White; White |]
let _red_face = [| Red; Red; Red; Red; Red; Red; Red; Red; Red |]  
let __red_face = [| Red; Red; Red; Red; Red; Red; Red; Red; Red |]  
let _yellow_face = [| Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow |]
let __yellow_face = [| Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow |]
let _blue_face = [| Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue |] 
let __blue_face = [| Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue |] 
let _orange_face = [| Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange |]  
let __orange_face = [| Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange |]  
let _green_face = [| Green; Green; Green; Green; Green; Green; Green; Green; Green |]  
let __green_face = [| Green; Green; Green; Green; Green; Green; Green; Green; Green |]  
let _cube = [| _orange_face; _yellow_face; _green_face; _white_face; _red_face; _blue_face |] 
let __cube = [| __orange_face; __yellow_face; __green_face; __white_face; __red_face; __blue_face |] 

let d_tests = 
  [
    turn_test "testing one _ turn on the default cube" _cube (init_cube f'_turn 0);
      turn_test "testing two _ turns on the default cube" __cube (init_cube_two_turns f'_turn f'_turn 0);
      undo_test
        "one _ turn on the cube that has made a _ turn should be equivalent default cube (i.e., the state that hasn't \
         changed)"
         (check_undo f'_turn f_turn 1000);
      undo_test "four _ turns should return the cube to its original state"  (check_undo_four_turns f'_turn 100);
  ]

let _white_face = [| White; White; White; White; White; White; White; White; White |]
let __white_face = [| White; White; White; White; White; White; White; White; White |]
let _red_face = [| Red; Red; Red; Red; Red; Red; Red; Red; Red |]  
let __red_face = [| Red; Red; Red; Red; Red; Red; Red; Red; Red |]  
let _yellow_face = [| Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow |]
let __yellow_face = [| Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow |]
let _blue_face = [| Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue |] 
let __blue_face = [| Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue |] 
let _orange_face = [| Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange |]  
let __orange_face = [| Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange |]  
let _green_face = [| Green; Green; Green; Green; Green; Green; Green; Green; Green |]  
let __green_face = [| Green; Green; Green; Green; Green; Green; Green; Green; Green |]  
let _cube = [| _orange_face; _yellow_face; _green_face; _white_face; _red_face; _blue_face |] 
let __cube = [| __orange_face; __yellow_face; __green_face; __white_face; __red_face; __blue_face |] 

let d'_tests = 
  [
    turn_test "testing one _ turn on the default cube" _cube (init_cube f'_turn 0);
      turn_test "testing two _ turns on the default cube" __cube (init_cube_two_turns f'_turn f'_turn 0);
      undo_test
        "one _ turn on the cube that has made a _ turn should be equivalent default cube (i.e., the state that hasn't \
         changed)"
         (check_undo f'_turn f_turn 1000);
      undo_test "four _ turns should return the cube to its original state"  (check_undo_four_turns f'_turn 100);
  ]
  
let _white_face = [| White; White; White; White; White; White; White; White; White |]
let __white_face = [| White; White; White; White; White; White; White; White; White |]
let _red_face = [| Red; Red; Red; Red; Red; Red; Red; Red; Red |]  
let __red_face = [| Red; Red; Red; Red; Red; Red; Red; Red; Red |]  
let _yellow_face = [| Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow |]
let __yellow_face = [| Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow |]
let _blue_face = [| Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue |] 
let __blue_face = [| Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue |] 
let _orange_face = [| Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange |]  
let __orange_face = [| Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange |]  
let _green_face = [| Green; Green; Green; Green; Green; Green; Green; Green; Green |]  
let __green_face = [| Green; Green; Green; Green; Green; Green; Green; Green; Green |]  
let _cube = [| _orange_face; _yellow_face; _green_face; _white_face; _red_face; _blue_face |] 
let __cube = [| __orange_face; __yellow_face; __green_face; __white_face; __red_face; __blue_face |] 

let b_tests = 
  [
    turn_test "testing one _ turn on the default cube" _cube (init_cube f'_turn 0);
      turn_test "testing two _ turns on the default cube" __cube (init_cube_two_turns f'_turn f'_turn 0);
      undo_test
        "one _ turn on the cube that has made a _ turn should be equivalent default cube (i.e., the state that hasn't \
         changed)"
         (check_undo f'_turn f_turn 1000);
      undo_test "four _ turns should return the cube to its original state"  (check_undo_four_turns f'_turn 100);
  ]

let _white_face = [| White; White; White; White; White; White; White; White; White |]
let __white_face = [| White; White; White; White; White; White; White; White; White |]
let _red_face = [| Red; Red; Red; Red; Red; Red; Red; Red; Red |]  
let __red_face = [| Red; Red; Red; Red; Red; Red; Red; Red; Red |]  
let _yellow_face = [| Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow |]
let __yellow_face = [| Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow |]
let _blue_face = [| Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue |] 
let __blue_face = [| Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue |] 
let _orange_face = [| Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange |]  
let __orange_face = [| Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange |]  
let _green_face = [| Green; Green; Green; Green; Green; Green; Green; Green; Green |]  
let __green_face = [| Green; Green; Green; Green; Green; Green; Green; Green; Green |]  
let _cube = [| _orange_face; _yellow_face; _green_face; _white_face; _red_face; _blue_face |] 
let __cube = [| __orange_face; __yellow_face; __green_face; __white_face; __red_face; __blue_face |] 

let b'_tests = 
  [
    turn_test "testing one _ turn on the default cube" _cube (init_cube f'_turn 0);
      turn_test "testing two _ turns on the default cube" __cube (init_cube_two_turns f'_turn f'_turn 0);
      undo_test
        "one _ turn on the cube that has made a _ turn should be equivalent default cube (i.e., the state that hasn't \
         changed)"
         (check_undo f'_turn f_turn 1000);
      undo_test "four _ turns should return the cube to its original state"  (check_undo_four_turns f'_turn 100);
  ]

let _white_face = [| White; White; White; White; White; White; White; White; White |]
let __white_face = [| White; White; White; White; White; White; White; White; White |]
let _red_face = [| Red; Red; Red; Red; Red; Red; Red; Red; Red |]  
let __red_face = [| Red; Red; Red; Red; Red; Red; Red; Red; Red |]  
let _yellow_face = [| Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow |]
let __yellow_face = [| Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow |]
let _blue_face = [| Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue |] 
let __blue_face = [| Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue |] 
let _orange_face = [| Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange |]  
let __orange_face = [| Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange |]  
let _green_face = [| Green; Green; Green; Green; Green; Green; Green; Green; Green |]  
let __green_face = [| Green; Green; Green; Green; Green; Green; Green; Green; Green |]  
let _cube = [| _orange_face; _yellow_face; _green_face; _white_face; _red_face; _blue_face |] 
let __cube = [| __orange_face; __yellow_face; __green_face; __white_face; __red_face; __blue_face |] 

let r_tests = 
  [
    turn_test "testing one _ turn on the default cube" _cube (init_cube f'_turn 0);
      turn_test "testing two _ turns on the default cube" __cube (init_cube_two_turns f'_turn f'_turn 0);
      undo_test
        "one _ turn on the cube that has made a _ turn should be equivalent default cube (i.e., the state that hasn't \
         changed)"
         (check_undo f'_turn f_turn 1000);
      undo_test "four _ turns should return the cube to its original state"  (check_undo_four_turns f'_turn 100);
  ]

let _white_face = [| White; White; White; White; White; White; White; White; White |]
let __white_face = [| White; White; White; White; White; White; White; White; White |]
let _red_face = [| Red; Red; Red; Red; Red; Red; Red; Red; Red |]  
let __red_face = [| Red; Red; Red; Red; Red; Red; Red; Red; Red |]  
let _yellow_face = [| Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow |]
let __yellow_face = [| Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow |]
let _blue_face = [| Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue |] 
let __blue_face = [| Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue |] 
let _orange_face = [| Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange |]  
let __orange_face = [| Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange |]  
let _green_face = [| Green; Green; Green; Green; Green; Green; Green; Green; Green |]  
let __green_face = [| Green; Green; Green; Green; Green; Green; Green; Green; Green |]  
let _cube = [| _orange_face; _yellow_face; _green_face; _white_face; _red_face; _blue_face |] 
let __cube = [| __orange_face; __yellow_face; __green_face; __white_face; __red_face; __blue_face |] 

let r'_tests = 
  [
    turn_test "testing one _ turn on the default cube" _cube (init_cube f'_turn 0);
      turn_test "testing two _ turns on the default cube" __cube (init_cube_two_turns f'_turn f'_turn 0);
      undo_test
        "one _ turn on the cube that has made a _ turn should be equivalent default cube (i.e., the state that hasn't \
         changed)"
         (check_undo f'_turn f_turn 1000);
      undo_test "four _ turns should return the cube to its original state"  (check_undo_four_turns f'_turn 100);
  ]

let _white_face = [| White; White; White; White; White; White; White; White; White |]
let __white_face = [| White; White; White; White; White; White; White; White; White |]
let _red_face = [| Red; Red; Red; Red; Red; Red; Red; Red; Red |]  
let __red_face = [| Red; Red; Red; Red; Red; Red; Red; Red; Red |]  
let _yellow_face = [| Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow |]
let __yellow_face = [| Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow |]
let _blue_face = [| Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue |] 
let __blue_face = [| Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue |] 
let _orange_face = [| Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange |]  
let __orange_face = [| Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange |]  
let _green_face = [| Green; Green; Green; Green; Green; Green; Green; Green; Green |]  
let __green_face = [| Green; Green; Green; Green; Green; Green; Green; Green; Green |]  
let _cube = [| _orange_face; _yellow_face; _green_face; _white_face; _red_face; _blue_face |] 
let __cube = [| __orange_face; __yellow_face; __green_face; __white_face; __red_face; __blue_face |] 

let l_tests = 
  [
    turn_test "testing one _ turn on the default cube" _cube (init_cube f'_turn 0);
      turn_test "testing two _ turns on the default cube" __cube (init_cube_two_turns f'_turn f'_turn 0);
      undo_test
        "one _ turn on the cube that has made a _ turn should be equivalent default cube (i.e., the state that hasn't \
         changed)"
         (check_undo f'_turn f_turn 1000);
      undo_test "four _ turns should return the cube to its original state"  (check_undo_four_turns f'_turn 100);
  ]

let _white_face = [| White; White; White; White; White; White; White; White; White |]
let __white_face = [| White; White; White; White; White; White; White; White; White |]
let _red_face = [| Red; Red; Red; Red; Red; Red; Red; Red; Red |]  
let __red_face = [| Red; Red; Red; Red; Red; Red; Red; Red; Red |]  
let _yellow_face = [| Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow |]
let __yellow_face = [| Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow |]
let _blue_face = [| Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue |] 
let __blue_face = [| Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue |] 
let _orange_face = [| Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange |]  
let __orange_face = [| Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange |]  
let _green_face = [| Green; Green; Green; Green; Green; Green; Green; Green; Green |]  
let __green_face = [| Green; Green; Green; Green; Green; Green; Green; Green; Green |]  
let _cube = [| _orange_face; _yellow_face; _green_face; _white_face; _red_face; _blue_face |] 
let __cube = [| __orange_face; __yellow_face; __green_face; __white_face; __red_face; __blue_face |] 

let l'_tests = 
  [
    turn_test "testing one _ turn on the default cube" _cube (init_cube f'_turn 0);
      turn_test "testing two _ turns on the default cube" __cube (init_cube_two_turns f'_turn f'_turn 0);
      undo_test
        "one _ turn on the cube that has made a _ turn should be equivalent default cube (i.e., the state that hasn't \
         changed)"
         (check_undo f'_turn f_turn 1000);
      undo_test "four _ turns should return the cube to its original state"  (check_undo_four_turns f'_turn 100);
  ]

let _white_face = [| White; White; White; White; White; White; White; White; White |]
let __white_face = [| White; White; White; White; White; White; White; White; White |]
let _red_face = [| Red; Red; Red; Red; Red; Red; Red; Red; Red |]  
let __red_face = [| Red; Red; Red; Red; Red; Red; Red; Red; Red |]  
let _yellow_face = [| Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow |]
let __yellow_face = [| Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow |]
let _blue_face = [| Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue |] 
let __blue_face = [| Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue |] 
let _orange_face = [| Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange |]  
let __orange_face = [| Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange |]  
let _green_face = [| Green; Green; Green; Green; Green; Green; Green; Green; Green |]  
let __green_face = [| Green; Green; Green; Green; Green; Green; Green; Green; Green |]  
let _cube = [| _orange_face; _yellow_face; _green_face; _white_face; _red_face; _blue_face |] 
let __cube = [| __orange_face; __yellow_face; __green_face; __white_face; __red_face; __blue_face |] 

let m_tests = 
  [
    turn_test "testing one _ turn on the default cube" _cube (init_cube f'_turn 0);
      turn_test "testing two _ turns on the default cube" __cube (init_cube_two_turns f'_turn f'_turn 0);
      undo_test
        "one _ turn on the cube that has made a _ turn should be equivalent default cube (i.e., the state that hasn't \
         changed)"
         (check_undo f'_turn f_turn 1000);
      undo_test "four _ turns should return the cube to its original state"  (check_undo_four_turns f'_turn 100);
  ]

let _white_face = [| White; White; White; White; White; White; White; White; White |]
let __white_face = [| White; White; White; White; White; White; White; White; White |]
let _red_face = [| Red; Red; Red; Red; Red; Red; Red; Red; Red |]  
let __red_face = [| Red; Red; Red; Red; Red; Red; Red; Red; Red |]  
let _yellow_face = [| Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow |]
let __yellow_face = [| Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow |]
let _blue_face = [| Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue |] 
let __blue_face = [| Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue |] 
let _orange_face = [| Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange |]  
let __orange_face = [| Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange |]  
let _green_face = [| Green; Green; Green; Green; Green; Green; Green; Green; Green |]  
let __green_face = [| Green; Green; Green; Green; Green; Green; Green; Green; Green |]  
let _cube = [| _orange_face; _yellow_face; _green_face; _white_face; _red_face; _blue_face |] 
let __cube = [| __orange_face; __yellow_face; __green_face; __white_face; __red_face; __blue_face |] 

let m'_tests = 
  [
    turn_test "testing one _ turn on the default cube" _cube (init_cube f'_turn 0);
      turn_test "testing two _ turns on the default cube" __cube (init_cube_two_turns f'_turn f'_turn 0);
      undo_test
        "one _ turn on the cube that has made a _ turn should be equivalent default cube (i.e., the state that hasn't \
         changed)"
         (check_undo f'_turn f_turn 1000);
      undo_test "four _ turns should return the cube to its original state"  (check_undo_four_turns f'_turn 100);
  ]

let _white_face = [| White; White; White; White; White; White; White; White; White |]
let __white_face = [| White; White; White; White; White; White; White; White; White |]
let _red_face = [| Red; Red; Red; Red; Red; Red; Red; Red; Red |]  
let __red_face = [| Red; Red; Red; Red; Red; Red; Red; Red; Red |]  
let _yellow_face = [| Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow |]
let __yellow_face = [| Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow |]
let _blue_face = [| Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue |] 
let __blue_face = [| Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue |] 
let _orange_face = [| Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange |]  
let __orange_face = [| Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange |]  
let _green_face = [| Green; Green; Green; Green; Green; Green; Green; Green; Green |]  
let __green_face = [| Green; Green; Green; Green; Green; Green; Green; Green; Green |]  
let _cube = [| _orange_face; _yellow_face; _green_face; _white_face; _red_face; _blue_face |] 
let __cube = [| __orange_face; __yellow_face; __green_face; __white_face; __red_face; __blue_face |] 

let e_tests = 
  [
    turn_test "testing one _ turn on the default cube" _cube (init_cube f'_turn 0);
      turn_test "testing two _ turns on the default cube" __cube (init_cube_two_turns f'_turn f'_turn 0);
      undo_test
        "one _ turn on the cube that has made a _ turn should be equivalent default cube (i.e., the state that hasn't \
         changed)"
         (check_undo f'_turn f_turn 1000);
      undo_test "four _ turns should return the cube to its original state"  (check_undo_four_turns f'_turn 100);
  ]

let _white_face = [| White; White; White; White; White; White; White; White; White |]
let __white_face = [| White; White; White; White; White; White; White; White; White |]
let _red_face = [| Red; Red; Red; Red; Red; Red; Red; Red; Red |]  
let __red_face = [| Red; Red; Red; Red; Red; Red; Red; Red; Red |]  
let _yellow_face = [| Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow |]
let __yellow_face = [| Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow |]
let _blue_face = [| Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue |] 
let __blue_face = [| Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue |] 
let _orange_face = [| Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange |]  
let __orange_face = [| Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange |]  
let _green_face = [| Green; Green; Green; Green; Green; Green; Green; Green; Green |]  
let __green_face = [| Green; Green; Green; Green; Green; Green; Green; Green; Green |]  
let _cube = [| _orange_face; _yellow_face; _green_face; _white_face; _red_face; _blue_face |] 
let __cube = [| __orange_face; __yellow_face; __green_face; __white_face; __red_face; __blue_face |] 

let e'_tests = 
  [
    turn_test "testing one _ turn on the default cube" _cube (init_cube f'_turn 0);
      turn_test "testing two _ turns on the default cube" __cube (init_cube_two_turns f'_turn f'_turn 0);
      undo_test
        "one _ turn on the cube that has made a _ turn should be equivalent default cube (i.e., the state that hasn't \
         changed)"
         (check_undo f'_turn f_turn 1000);
      undo_test "four _ turns should return the cube to its original state"  (check_undo_four_turns f'_turn 100);
  ]

let _white_face = [| White; White; White; White; White; White; White; White; White |]
let __white_face = [| White; White; White; White; White; White; White; White; White |]
let _red_face = [| Red; Red; Red; Red; Red; Red; Red; Red; Red |]  
let __red_face = [| Red; Red; Red; Red; Red; Red; Red; Red; Red |]  
let _yellow_face = [| Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow |]
let __yellow_face = [| Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow |]
let _blue_face = [| Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue |] 
let __blue_face = [| Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue |] 
let _orange_face = [| Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange |]  
let __orange_face = [| Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange |]  
let _green_face = [| Green; Green; Green; Green; Green; Green; Green; Green; Green |]  
let __green_face = [| Green; Green; Green; Green; Green; Green; Green; Green; Green |]  
let _cube = [| _orange_face; _yellow_face; _green_face; _white_face; _red_face; _blue_face |] 
let __cube = [| __orange_face; __yellow_face; __green_face; __white_face; __red_face; __blue_face |] 

let s_tests = 
  [
    turn_test "testing one _ turn on the default cube" _cube (init_cube f'_turn 0);
      turn_test "testing two _ turns on the default cube" __cube (init_cube_two_turns f'_turn f'_turn 0);
      undo_test
        "one _ turn on the cube that has made a _ turn should be equivalent default cube (i.e., the state that hasn't \
         changed)"
         (check_undo f'_turn f_turn 1000);
      undo_test "four _ turns should return the cube to its original state"  (check_undo_four_turns f'_turn 100);
  ]

let _white_face = [| White; White; White; White; White; White; White; White; White |]
let __white_face = [| White; White; White; White; White; White; White; White; White |]
let _red_face = [| Red; Red; Red; Red; Red; Red; Red; Red; Red |]  
let __red_face = [| Red; Red; Red; Red; Red; Red; Red; Red; Red |]  
let _yellow_face = [| Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow |]
let __yellow_face = [| Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow |]
let _blue_face = [| Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue |] 
let __blue_face = [| Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue |] 
let _orange_face = [| Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange |]  
let __orange_face = [| Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange |]  
let _green_face = [| Green; Green; Green; Green; Green; Green; Green; Green; Green |]  
let __green_face = [| Green; Green; Green; Green; Green; Green; Green; Green; Green |]  
let _cube = [| _orange_face; _yellow_face; _green_face; _white_face; _red_face; _blue_face |] 
let __cube = [| __orange_face; __yellow_face; __green_face; __white_face; __red_face; __blue_face |] 

let s'_tests = 
  [
    turn_test "testing one _ turn on the default cube" _cube (init_cube f'_turn 0);
      turn_test "testing two _ turns on the default cube" __cube (init_cube_two_turns f'_turn f'_turn 0);
      undo_test
        "one _ turn on the cube that has made a _ turn should be equivalent default cube (i.e., the state that hasn't \
         changed)"
         (check_undo f'_turn f_turn 1000);
      undo_test "four _ turns should return the cube to its original state"  (check_undo_four_turns f'_turn 100);
  ]

let _white_face = [| White; White; White; White; White; White; White; White; White |]
let __white_face = [| White; White; White; White; White; White; White; White; White |]
let _red_face = [| Red; Red; Red; Red; Red; Red; Red; Red; Red |]  
let __red_face = [| Red; Red; Red; Red; Red; Red; Red; Red; Red |]  
let _yellow_face = [| Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow |]
let __yellow_face = [| Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow |]
let _blue_face = [| Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue |] 
let __blue_face = [| Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue |] 
let _orange_face = [| Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange |]  
let __orange_face = [| Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange |]  
let _green_face = [| Green; Green; Green; Green; Green; Green; Green; Green; Green |]  
let __green_face = [| Green; Green; Green; Green; Green; Green; Green; Green; Green |]  
let _cube = [| _orange_face; _yellow_face; _green_face; _white_face; _red_face; _blue_face |] 
let __cube = [| __orange_face; __yellow_face; __green_face; __white_face; __red_face; __blue_face |] 

let y_tests = 
  [
    turn_test "testing one _ turn on the default cube" _cube (init_cube f'_turn 0);
      turn_test "testing two _ turns on the default cube" __cube (init_cube_two_turns f'_turn f'_turn 0);
      undo_test
        "one _ turn on the cube that has made a _ turn should be equivalent default cube (i.e., the state that hasn't \
         changed)"
         (check_undo f'_turn f_turn 1000);
      undo_test "four _ turns should return the cube to its original state"  (check_undo_four_turns f'_turn 100);
  ]

let _white_face = [| White; White; White; White; White; White; White; White; White |]
let __white_face = [| White; White; White; White; White; White; White; White; White |]
let _red_face = [| Red; Red; Red; Red; Red; Red; Red; Red; Red |]  
let __red_face = [| Red; Red; Red; Red; Red; Red; Red; Red; Red |]  
let _yellow_face = [| Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow |]
let __yellow_face = [| Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow |]
let _blue_face = [| Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue |] 
let __blue_face = [| Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue |] 
let _orange_face = [| Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange |]  
let __orange_face = [| Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange |]  
let _green_face = [| Green; Green; Green; Green; Green; Green; Green; Green; Green |]  
let __green_face = [| Green; Green; Green; Green; Green; Green; Green; Green; Green |]  
let _cube = [| _orange_face; _yellow_face; _green_face; _white_face; _red_face; _blue_face |] 
let __cube = [| __orange_face; __yellow_face; __green_face; __white_face; __red_face; __blue_face |] 

let y'_tests = 
  [
    turn_test "testing one _ turn on the default cube" _cube (init_cube f'_turn 0);
      turn_test "testing two _ turns on the default cube" __cube (init_cube_two_turns f'_turn f'_turn 0);
      undo_test
        "one _ turn on the cube that has made a _ turn should be equivalent default cube (i.e., the state that hasn't \
         changed)"
         (check_undo f'_turn f_turn 1000);
      undo_test "four _ turns should return the cube to its original state"  (check_undo_four_turns f'_turn 100);
  ]

let _white_face = [| White; White; White; White; White; White; White; White; White |]
let __white_face = [| White; White; White; White; White; White; White; White; White |]
let _red_face = [| Red; Red; Red; Red; Red; Red; Red; Red; Red |]  
let __red_face = [| Red; Red; Red; Red; Red; Red; Red; Red; Red |]  
let _yellow_face = [| Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow |]
let __yellow_face = [| Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow |]
let _blue_face = [| Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue |] 
let __blue_face = [| Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue |] 
let _orange_face = [| Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange |]  
let __orange_face = [| Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange |]  
let _green_face = [| Green; Green; Green; Green; Green; Green; Green; Green; Green |]  
let __green_face = [| Green; Green; Green; Green; Green; Green; Green; Green; Green |]  
let _cube = [| _orange_face; _yellow_face; _green_face; _white_face; _red_face; _blue_face |] 
let __cube = [| __orange_face; __yellow_face; __green_face; __white_face; __red_face; __blue_face |] 

let x_tests = 
  [
    turn_test "testing one _ turn on the default cube" _cube (init_cube f'_turn 0);
      turn_test "testing two _ turns on the default cube" __cube (init_cube_two_turns f'_turn f'_turn 0);
      undo_test
        "one _ turn on the cube that has made a _ turn should be equivalent default cube (i.e., the state that hasn't \
         changed)"
         (check_undo f'_turn f_turn 1000);
      undo_test "four _ turns should return the cube to its original state"  (check_undo_four_turns f'_turn 100);
  ]

let _white_face = [| White; White; White; White; White; White; White; White; White |]
let __white_face = [| White; White; White; White; White; White; White; White; White |]
let _red_face = [| Red; Red; Red; Red; Red; Red; Red; Red; Red |]  
let __red_face = [| Red; Red; Red; Red; Red; Red; Red; Red; Red |]  
let _yellow_face = [| Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow |]
let __yellow_face = [| Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow |]
let _blue_face = [| Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue |] 
let __blue_face = [| Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue |] 
let _orange_face = [| Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange |]  
let __orange_face = [| Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange |]  
let _green_face = [| Green; Green; Green; Green; Green; Green; Green; Green; Green |]  
let __green_face = [| Green; Green; Green; Green; Green; Green; Green; Green; Green |]  
let _cube = [| _orange_face; _yellow_face; _green_face; _white_face; _red_face; _blue_face |] 
let __cube = [| __orange_face; __yellow_face; __green_face; __white_face; __red_face; __blue_face |] 

let x'_tests = 
  [
    turn_test "testing one _ turn on the default cube" _cube (init_cube f'_turn 0);
      turn_test "testing two _ turns on the default cube" __cube (init_cube_two_turns f'_turn f'_turn 0);
      undo_test
        "one _ turn on the cube that has made a _ turn should be equivalent default cube (i.e., the state that hasn't \
         changed)"
         (check_undo f'_turn f_turn 1000);
      undo_test "four _ turns should return the cube to its original state"  (check_undo_four_turns f'_turn 100);
  ]

let _white_face = [| White; White; White; White; White; White; White; White; White |]
let __white_face = [| White; White; White; White; White; White; White; White; White |]
let _red_face = [| Red; Red; Red; Red; Red; Red; Red; Red; Red |]  
let __red_face = [| Red; Red; Red; Red; Red; Red; Red; Red; Red |]  
let _yellow_face = [| Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow |]
let __yellow_face = [| Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow |]
let _blue_face = [| Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue |] 
let __blue_face = [| Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue |] 
let _orange_face = [| Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange |]  
let __orange_face = [| Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange |]  
let _green_face = [| Green; Green; Green; Green; Green; Green; Green; Green; Green |]  
let __green_face = [| Green; Green; Green; Green; Green; Green; Green; Green; Green |]  
let _cube = [| _orange_face; _yellow_face; _green_face; _white_face; _red_face; _blue_face |] 
let __cube = [| __orange_face; __yellow_face; __green_face; __white_face; __red_face; __blue_face |] 

let z_tests = 
  [
    turn_test "testing one _ turn on the default cube" _cube (init_cube f'_turn 0);
      turn_test "testing two _ turns on the default cube" __cube (init_cube_two_turns f'_turn f'_turn 0);
      undo_test
        "one _ turn on the cube that has made a _ turn should be equivalent default cube (i.e., the state that hasn't \
         changed)"
         (check_undo f'_turn f_turn 1000);
      undo_test "four _ turns should return the cube to its original state"  (check_undo_four_turns f'_turn 100);
  ]

let _white_face = [| White; White; White; White; White; White; White; White; White |]
let __white_face = [| White; White; White; White; White; White; White; White; White |]
let _red_face = [| Red; Red; Red; Red; Red; Red; Red; Red; Red |]  
let __red_face = [| Red; Red; Red; Red; Red; Red; Red; Red; Red |]  
let _yellow_face = [| Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow |]
let __yellow_face = [| Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Yellow |]
let _blue_face = [| Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue |] 
let __blue_face = [| Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue; Blue |] 
let _orange_face = [| Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange |]  
let __orange_face = [| Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange; Orange |]  
let _green_face = [| Green; Green; Green; Green; Green; Green; Green; Green; Green |]  
let __green_face = [| Green; Green; Green; Green; Green; Green; Green; Green; Green |]  
let _cube = [| _orange_face; _yellow_face; _green_face; _white_face; _red_face; _blue_face |] 
let __cube = [| __orange_face; __yellow_face; __green_face; __white_face; __red_face; __blue_face |] 

let z'_tests = 
  [
    turn_test "testing one _ turn on the default cube" _cube (init_cube f'_turn 0);
      turn_test "testing two _ turns on the default cube" __cube (init_cube_two_turns f'_turn f'_turn 0);
      undo_test
        "one _ turn on the cube that has made a _ turn should be equivalent default cube (i.e., the state that hasn't \
         changed)"
         (check_undo f'_turn f_turn 1000);
      undo_test "four _ turns should return the cube to its original state"  (check_undo_four_turns f'_turn 100);
  ]

let suite = "test suite for Cube" >::: List.flatten [ u'_tests; u_tests; f_tests; f'_tests; d_tests; d'_tests; b_tests; b'_tests;
r_tests; r'_tests; l_tests; l'_tests; m_tests; m'_tests; e_tests; e'_tests; s_tests; s'_tests; y_tests; y'_tests; x_tests; x'_tests;
z_tests; z'_tests ]
let _ = run_test_tt_main suite
