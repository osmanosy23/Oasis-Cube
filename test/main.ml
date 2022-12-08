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
    
let d_red_face = [| Blue; Blue; Blue; Red; Red; Red; Red; Red; Red |]  
let d_d_red_face = [| Orange; Orange; Orange; Red; Red; Red; Red; Red; Red |]  
let d_blue_face = [| Orange; Orange; Orange; Blue; Blue; Blue; Blue; Blue; Blue |] 
let d_d_blue_face = [| Green; Green; Green; Blue; Blue; Blue; Blue; Blue; Blue |] 
let d_orange_face = [| Green; Green; Green; Orange; Orange; Orange; Orange; Orange; Orange |]  
let d_d_orange_face = [| Red; Red; Red; Orange; Orange; Orange; Orange; Orange; Orange |]  
let d_green_face = [| Red; Red; Red; Green; Green; Green; Green; Green; Green |]  
let d_d_green_face = [| Blue; Blue; Blue; Green; Green; Green; Green; Green; Green |]  
let d_cube = [| d_orange_face; yellow_face; d_green_face; white_face; d_red_face; d_blue_face |] 
let d_d_cube = [| d_d_orange_face; yellow_face; d_d_green_face; white_face; d_d_red_face; d_d_blue_face |] 

let d_tests = 
  [
    turn_test "testing one d turn on the default cube" d_cube (init_cube d_turn 0);
      turn_test "testing two d turns on the default cube" d_d_cube (init_cube_two_turns d_turn d_turn 0);
      undo_test
        "one d' turn on the cube that has made a d turn should be equivalent default cube (i.e., the state that hasn't \
         changed)"
         (check_undo d'_turn d_turn 1000);
      undo_test "four d turns should return the cube to its original state"  (check_undo_four_turns d_turn 100);
  ]

  let d'_red_face = [| Green; Green; Green; Red; Red; Red; Red; Red; Red |]  
  let d'_d'_red_face = [| Orange; Orange; Orange; Red; Red; Red; Red; Red; Red |]  
  let d'_blue_face = [| Red; Red; Red; Blue; Blue; Blue; Blue; Blue; Blue |] 
  let d'_d'_blue_face = [| Green; Green; Green; Blue; Blue; Blue; Blue; Blue; Blue |] 
  let d'_orange_face = [| Blue; Blue; Blue; Orange; Orange; Orange; Orange; Orange; Orange |]  
  let d'_d'_orange_face = [| Red; Red; Red; Orange; Orange; Orange; Orange; Orange; Orange |]  
  let d'_green_face = [| Orange; Orange; Orange; Green; Green; Green; Green; Green; Green |]  
  let d'_d'_green_face = [| Blue; Blue; Blue; Green; Green; Green; Green; Green; Green |]  
  let d'_cube = [| d'_orange_face; yellow_face; d'_green_face; white_face; d'_red_face; d'_blue_face |] 
  let d'_d'_cube = [| d'_d'_orange_face; yellow_face; d'_d'_green_face; white_face; d'_d'_red_face; d'_d'_blue_face |] 
  

let d'_tests = 
  [
    turn_test "testing one d' turn on the default cube" d'_cube (init_cube d'_turn 0);
      turn_test "testing two d' turns on the default cube" d'_d'_cube (init_cube_two_turns d'_turn d'_turn 0);
      undo_test
        "one d turn on the cube that has made a d' turn should be equivalent default cube (i.e., the state that hasn't \
         changed)"
         (check_undo d_turn d'_turn 1000);
      undo_test "four d' turns should return the cube to its original state"  (check_undo_four_turns d'_turn 100);
  ]
  
let b_white_face = [| Blue; Blue; Blue; White; White; White; White; White; White |]
let b_b_white_face = [| Yellow; Yellow; Yellow; White; White; White; White; White; White |]
let b_yellow_face = [| Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Green; Green; Green |]
let b_b_yellow_face = [| Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; White; White; White |]
let b_blue_face = [| Yellow; Blue; Blue; Yellow; Blue; Blue; Yellow; Blue; Blue |] 
let b_b_blue_face = [| Green; Blue; Blue; Green; Blue; Blue; Green; Blue; Blue |] 
let b_green_face = [| Green; Green; White; Green; Green; White; Green; Green; White |]  
let b_b_green_face = [| Green; Green; Blue; Green; Green; Blue; Green; Green; Blue |]  
let b_cube = [| orange_face; b_yellow_face; b_green_face; b_white_face; red_face; b_blue_face |] 
let b_b_cube = [| orange_face; b_b_yellow_face; b_b_green_face; b_b_white_face; red_face; b_b_blue_face |] 

let b_tests = 
  [
    turn_test "testing one b turn on the default cube" b_cube (init_cube b_turn 0);
      turn_test "testing two b turns on the default cube" b_b_cube (init_cube_two_turns b_turn b_turn 0);
      undo_test
        "one b' turn on the cube that has made a b turn should be equivalent default cube (i.e., the state that hasn't \
         changed)"
         (check_undo b'_turn b_turn 1000);
      undo_test "four b turns should return the cube to its original state"  (check_undo_four_turns b_turn 100);
  ]

let b'_white_face = [| Green; Green; Green; White; White; White; White; White; White |]
let b'_b'_white_face = [| Yellow; Yellow; Yellow; White; White; White; White; White; White |]
let b'_yellow_face = [| Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; Blue; Blue; Blue |]
let b'_b'_yellow_face = [| Yellow; Yellow; Yellow; Yellow; Yellow; Yellow; White; White; White |]
let b'_blue_face = [| White; Blue; Blue; White; Blue; Blue; White; Blue; Blue |] 
let b'_b'_blue_face = [| Green; Blue; Blue; Green; Blue; Blue; Green; Blue; Blue |] 
let b'_green_face = [| Green; Green; Yellow; Green; Green; Yellow; Green; Green; Yellow |]  
let b'_b'_green_face = [| Green; Green; Blue; Green; Green; Blue; Green; Green; Blue |]  
let b'_cube = [| orange_face; b'_yellow_face; b'_green_face; b'_white_face; red_face; b'_blue_face |] 
let b'_b'_cube = [| orange_face; b'_b'_yellow_face; b'_b'_green_face; b'_b'_white_face; red_face; b'_b'_blue_face |] 

let b'_tests = 
  [
    turn_test "testing one b' turn on the default cube" b'_cube (init_cube b'_turn 0);
      turn_test "testing two b' turns on the default cube" b'_b'_cube (init_cube_two_turns b'_turn b'_turn 0);
      undo_test
        "one b turn on the cube that has made a b' turn should be equivalent default cube (i.e., the state that hasn't \
         changed)"
         (check_undo b_turn b'_turn 1000);
      undo_test "four b' turns should return the cube to its original state"  (check_undo_four_turns b'_turn 100);
  ]

let r_white_face = [| White; White; Orange; White; White; Orange; White; White; Orange |]
let r_r_white_face = [| White; White; Yellow; White; White; Yellow; White; White; Yellow |]
let r_red_face = [| Red; Red; White; Red; Red; White; Red; Red; White |]  
let r_r_red_face = [| Red; Red; Orange; Red; Red; Orange; Red; Red; Orange |]  
let r_yellow_face = [| Yellow; Yellow; Red; Yellow; Yellow; Red; Yellow; Yellow; Red |]
let r_r_yellow_face = [| Yellow; Yellow; White; Yellow; Yellow; White; Yellow; Yellow; White |]
let r_orange_face = [| Yellow; Orange; Orange; Yellow; Orange; Orange; Yellow; Orange; Orange |]  
let r_r_orange_face = [| Red; Orange; Orange; Red; Orange; Orange; Red; Orange; Orange |]  
let r_cube = [| r_orange_face; r_yellow_face; green_face; r_white_face; r_red_face; blue_face |] 
let r_r_cube = [| r_r_orange_face; r_r_yellow_face; green_face; r_r_white_face; r_r_red_face; blue_face |] 

let r_tests = 
  [
    turn_test "testing one r turn on the default cube" r_cube (init_cube r_turn 0);
      turn_test "testing two r turns on the default cube" r_r_cube (init_cube_two_turns r_turn r_turn 0);
      undo_test
        "one r' turn on the cube that has made a r turn should be equivalent default cube (i.e., the state that hasn't \
         changed)"
         (check_undo r'_turn r_turn 1000);
      undo_test "four r turns should return the cube to its original state"  (check_undo_four_turns r_turn 100);
  ]

  let r'_white_face = [| White; White; Red; White; White; Red; White; White; Red |]
  let r'_r'_white_face = [| White; White; Yellow; White; White; Yellow; White; White; Yellow |]
  let r'_red_face = [| Red; Red; Yellow; Red; Red; Yellow; Red; Red; Yellow |]  
  let r'_r'_red_face = [| Red; Red; Orange; Red; Red; Orange; Red; Red; Orange |]  
  let r'_yellow_face = [| Yellow; Yellow; Orange; Yellow; Yellow; Orange; Yellow; Yellow; Orange |]
  let r'_r'_yellow_face = [| Yellow; Yellow; White; Yellow; Yellow; White; Yellow; Yellow; White |]
  let r'_orange_face = [| White; Orange; Orange; White; Orange; Orange; White; Orange; Orange |]  
  let r'_r'_orange_face = [| Red; Orange; Orange; Red; Orange; Orange; Red; Orange; Orange |]  
  let r'_cube = [| r'_orange_face; r'_yellow_face; green_face; r'_white_face; r'_red_face; blue_face |] 
  let r'_r'_cube = [| r'_r'_orange_face; r'_r'_yellow_face; green_face; r'_r'_white_face; r'_r'_red_face; blue_face |] 
  
  let r'_tests = 
    [
      turn_test "testing one r' turn on the default cube" r'_cube (init_cube r'_turn 0);
        turn_test "testing two r' turns on the default cube" r'_r'_cube (init_cube_two_turns r'_turn r'_turn 0);
        undo_test
          "one r turn on the cube that has made a r' turn should be equivalent default cube (i.e., the state that hasn't \
           changed)"
           (check_undo r_turn r'_turn 1000);
        undo_test "four r' turns should return the cube to its original state"  (check_undo_four_turns r'_turn 100);
    ]

let l_white_face = [| Red; White; White; Red; White; White; Red; White; White |]
let l_l_white_face = [| Yellow; White; White; Yellow; White; White; Yellow; White; White |]
let l_red_face = [| Yellow; Red; Red; Yellow; Red; Red; Yellow; Red; Red |]  
let l_l_red_face = [| Orange; Red; Red; Orange; Red; Red; Orange; Red; Red |]  
let l_yellow_face = [| Orange; Yellow; Yellow; Orange; Yellow; Yellow; Orange; Yellow; Yellow |]
let l_l_yellow_face = [| White; Yellow; Yellow; White; Yellow; Yellow; White; Yellow; Yellow |]
let l_orange_face = [| Orange; Orange; White; Orange; Orange; White; Orange; Orange; White |]  
let l_l_orange_face = [| Orange; Orange; Red; Orange; Orange; Red; Orange; Orange; Red |]  
let l_cube = [| l_orange_face; l_yellow_face; green_face; l_white_face; l_red_face; blue_face |] 
let l_l_cube = [| l_l_orange_face; l_l_yellow_face; green_face; l_l_white_face; l_l_red_face; blue_face |] 

let l_tests = 
  [
    turn_test "testing one l turn on the default cube" l_cube (init_cube l_turn 0);
      turn_test "testing two l turns on the default cube" l_l_cube (init_cube_two_turns l_turn l_turn 0);
      undo_test
        "one l' turn on the cube that has made a l turn should be equivalent default cube (i.e., the state that hasn't \
         changed)"
         (check_undo l'_turn l_turn 1000);
      undo_test "four l turns should return the cube to its original state"  (check_undo_four_turns l_turn 100);
  ]

let l'_white_face = [| Orange; White; White; Orange; White; White; Orange; White; White |]
let l'_l'_white_face = [| Yellow; White; White; Yellow; White; White; Yellow; White; White |]
let l'_red_face = [| White; Red; Red; White; Red; Red; White; Red; Red |]  
let l'_l'_red_face = [| Orange; Red; Red; Orange; Red; Red; Orange; Red; Red |]  
let l'_yellow_face = [| Red; Yellow; Yellow; Red; Yellow; Yellow; Red; Yellow; Yellow |]
let l'_l'_yellow_face = [| White; Yellow; Yellow; White; Yellow; Yellow; White; Yellow; Yellow |]
let l'_orange_face = [| Orange; Orange; Yellow; Orange; Orange; Yellow; Orange; Orange; Yellow |]  
let l'_l'_orange_face = [| Orange; Orange; Red; Orange; Orange; Red; Orange; Orange; Red |]  
let l'_cube = [| l'_orange_face; l'_yellow_face; green_face; l'_white_face; l'_red_face; blue_face |] 
let l'_l'_cube = [| l'_l'_orange_face; l'_l'_yellow_face; green_face; l'_l'_white_face; l'_l'_red_face; blue_face |] 

let l'_tests = 
  [
    turn_test "testing one l' turn on the default cube" l'_cube (init_cube l'_turn 0);
      turn_test "testing two l' turns on the default cube" l'_l'_cube (init_cube_two_turns l'_turn l'_turn 0);
      undo_test
        "one l turn on the cube that has made an l' turn should be equivalent default cube (i.e., the state that hasn't \
         changed)"
         (check_undo l_turn l'_turn 1000);
      undo_test "four l' turns should return the cube to its original state"  (check_undo_four_turns l'_turn 100);
  ]

let m_white_face = [| White; Red; White; White; Red; White; White; Red; White |]
let m_m_white_face = [| White; Yellow; White; White; Yellow; White; White; Yellow; White |]
let m_red_face = [| Red; Yellow; Red; Red; Yellow; Red; Red; Yellow; Red |]  
let m_m_red_face = [| Red; Orange; Red; Red; Orange; Red; Red; Orange; Red |]  
let m_yellow_face = [| Yellow; Orange; Yellow; Yellow; Orange; Yellow; Yellow; Orange; Yellow |]
let m_m_yellow_face = [| Yellow; White; Yellow; Yellow; White; Yellow; Yellow; White; Yellow |]
let m_orange_face = [| Orange; White; Orange; Orange; White; Orange; Orange; White; Orange |]  
let m_m_orange_face = [| Orange; Red; Orange; Orange; Red; Orange; Orange; Red; Orange |]  
let m_cube = [| m_orange_face; m_yellow_face; green_face; m_white_face; m_red_face; blue_face |] 
let m_m_cube = [| m_m_orange_face; m_m_yellow_face; green_face; m_m_white_face; m_m_red_face; blue_face |] 

let m_tests = 
  [
    turn_test "testing one m turn on the default cube" m_cube (init_cube m_turn 0);
      turn_test "testing two m turns on the default cube" m_m_cube (init_cube_two_turns m_turn m_turn 0);
      undo_test
        "one m' turn on the cube that has made an m turn should be equivalent default cube (i.e., the state that hasn't \
         changed)"
         (check_undo m'_turn m_turn 1000);
      undo_test "four m turns should return the cube to its original state"  (check_undo_four_turns m_turn 100);
  ]

let m'_white_face = [| White; Orange; White; White; Orange; White; White; Orange; White |]
let m'_m'_white_face = [| White; Yellow; White; White; Yellow; White; White; Yellow; White |]
let m'_red_face = [| Red; White; Red; Red; White; Red; Red; White; Red |]  
let m'_m'_red_face = [| Red; Orange; Red; Red; Orange; Red; Red; Orange; Red |]  
let m'_yellow_face = [| Yellow; Red; Yellow; Yellow; Red; Yellow; Yellow; Red; Yellow |]
let m'_m'_yellow_face = [| Yellow; White; Yellow; Yellow; White; Yellow; Yellow; White; Yellow |]
let m'_orange_face = [| Orange; Yellow; Orange; Orange; Yellow; Orange; Orange; Yellow; Orange |]  
let m'_m'_orange_face = [| Orange; Red; Orange; Orange; Red; Orange; Orange; Red; Orange |]  
let m'_cube = [| m'_orange_face; m'_yellow_face; green_face; m'_white_face; m'_red_face; blue_face |] 
let m'_m'_cube = [| m'_m'_orange_face; m'_m'_yellow_face; green_face; m'_m'_white_face; m'_m'_red_face; blue_face |] 
  
let m'_tests = 
  [
    turn_test "testing one m' turn on the default cube" m'_cube (init_cube m'_turn 0);
      turn_test "testing two m' turns on the default cube" m'_m'_cube (init_cube_two_turns m'_turn m'_turn 0);
      undo_test
        "one m turn on the cube that has made an m' turn should be equivalent default cube (i.e., the state that hasn't \
         changed)"
          (check_undo m_turn m'_turn 1000);
      undo_test "four m' turns should return the cube to its original state"  (check_undo_four_turns m'_turn 100);
  ]

let e_red_face = [| Red; Red; Red; Blue; Blue; Blue; Red; Red; Red |]  
let e_e_red_face = [| Red; Red; Red; Orange; Orange; Orange; Red; Red; Red |]  
let e_blue_face = [| Blue; Blue; Blue; Orange; Orange; Orange; Blue; Blue; Blue |] 
let e_e_blue_face = [| Blue; Blue; Blue; Green; Green; Green; Blue; Blue; Blue |] 
let e_orange_face = [| Orange; Orange; Orange; Green; Green; Green; Orange; Orange; Orange |]  
let e_e_orange_face = [| Orange; Orange; Orange; Red; Red; Red; Orange; Orange; Orange |]  
let e_green_face = [| Green; Green; Green; Red; Red; Red; Green; Green; Green |]  
let e_e_green_face = [| Green; Green; Green; Blue; Blue; Blue; Green; Green; Green |]  
let e_cube = [| e_orange_face; yellow_face; e_green_face; white_face; e_red_face; e_blue_face |] 
let e_e_cube = [| e_e_orange_face; yellow_face; e_e_green_face; white_face; e_e_red_face; e_e_blue_face |] 

let e_tests = 
  [
    turn_test "testing one e turn on the default cube" e_cube (init_cube e_turn 0);
      turn_test "testing two e turns on the default cube" e_e_cube (init_cube_two_turns e_turn e_turn 0);
      undo_test
        "one e' turn on the cube that has made an e turn should be equivalent default cube (i.e., the state that hasn't \
         changed)"
         (check_undo e'_turn e_turn 1000);
      undo_test "four e turns should return the cube to its original state"  (check_undo_four_turns e_turn 100);
  ]

let e'_red_face = [| Red; Red; Red; Green; Green; Green; Red; Red; Red |]  
let e'_e'_red_face = [| Red; Red; Red; Orange; Orange; Orange; Red; Red; Red |]  
let e'_blue_face = [| Blue; Blue; Blue; Red; Red; Red; Blue; Blue; Blue |] 
let e'_e'_blue_face = [| Blue; Blue; Blue; Green; Green; Green; Blue; Blue; Blue |] 
let e'_orange_face = [| Orange; Orange; Orange; Blue; Blue; Blue; Orange; Orange; Orange |]  
let e'_e'_orange_face = [| Orange; Orange; Orange; Red; Red; Red; Orange; Orange; Orange |]  
let e'_green_face = [| Green; Green; Green; Orange; Orange; Orange; Green; Green; Green |]  
let e'_e'_green_face = [| Green; Green; Green; Blue; Blue; Blue; Green; Green; Green |]  
let e'_cube = [| e'_orange_face; yellow_face; e'_green_face; white_face; e'_red_face; e'_blue_face |] 
let e'_e'_cube = [| e'_e'_orange_face; yellow_face; e'_e'_green_face; white_face; e'_e'_red_face; e'_e'_blue_face |] 

let e'_tests = 
  [
    turn_test "testing one e' turn on the default cube" e'_cube (init_cube e'_turn 0);
      turn_test "testing two e' turns on the default cube" e'_e'_cube (init_cube_two_turns e'_turn e'_turn 0);
      undo_test
        "one e turn on the cube that has made an e' turn should be equivalent default cube (i.e., the state that hasn't \
         changed)"
         (check_undo e_turn e'_turn 1000);
      undo_test "four e' turns should return the cube to its original state"  (check_undo_four_turns e'_turn 100);
  ]

let s_white_face = [| White; White; White; Green; Green; Green; White; White; White |]
let s_s_white_face = [| White; White; White; Yellow; Yellow; Yellow; White; White; White |]
let s_yellow_face = [| Yellow; Yellow; Yellow; Blue; Blue; Blue; Yellow; Yellow; Yellow |]
let s_s_yellow_face = [| Yellow; Yellow; Yellow; White; White; White; Yellow; Yellow; Yellow |]
let s_blue_face = [| Blue; White; Blue; Blue; White; Blue; Blue; White; Blue |] 
let s_s_blue_face = [| Blue; Green; Blue; Blue; Green; Blue; Blue; Green; Blue |] 
let s_green_face = [| Green; Yellow; Green; Green; Yellow; Green; Green; Yellow; Green |]  
let s_s_green_face = [| Green; Blue; Green; Green; Blue; Green; Green; Blue; Green |]  
let s_cube = [| orange_face; s_yellow_face; s_green_face; s_white_face; red_face; s_blue_face |] 
let s_s_cube = [| orange_face; s_s_yellow_face; s_s_green_face; s_s_white_face; red_face; s_s_blue_face |] 

let s_tests = 
  [
    turn_test "testing one s turn on the default cube" s_cube (init_cube s_turn 0);
      turn_test "testing two s turns on the default cube" s_s_cube (init_cube_two_turns s_turn s_turn 0);
      undo_test
        "one s' turn on the cube that has made an s turn should be equivalent default cube (i.e., the state that hasn't \
         changed)"
         (check_undo s'_turn s_turn 1000);
      undo_test "four s turns should return the cube to its original state"  (check_undo_four_turns s_turn 100);
  ]

let s'_white_face = [| White; White; White; Blue; Blue; Blue; White; White; White |]
let s'_s'_white_face = [| White; White; White; Yellow; Yellow; Yellow; White; White; White |]
let s'_yellow_face = [| Yellow; Yellow; Yellow; Green; Green; Green; Yellow; Yellow; Yellow |]
let s'_s'_yellow_face = [| Yellow; Yellow; Yellow; White; White; White; Yellow; Yellow; Yellow |]
let s'_blue_face = [| Blue; Yellow; Blue; Blue; Yellow; Blue; Blue; Yellow; Blue |] 
let s'_s'_blue_face = [| Blue; Green; Blue; Blue; Green; Blue; Blue; Green; Blue |] 
let s'_green_face = [| Green; White; Green; Green; White; Green; Green; White; Green |]  
let s'_s'_green_face = [| Green; Blue; Green; Green; Blue; Green; Green; Blue; Green |]  
let s'_cube = [| orange_face; s'_yellow_face; s'_green_face; s'_white_face; red_face; s'_blue_face |] 
let s'_s'_cube = [| orange_face; s'_s'_yellow_face; s'_s'_green_face; s'_s'_white_face; red_face; s'_s'_blue_face |] 

let s'_tests = 
  [
    turn_test "testing one s' turn on the default cube" s'_cube (init_cube s'_turn 0);
      turn_test "testing two s' turns on the default cube" s'_s'_cube (init_cube_two_turns s'_turn s'_turn 0);
      undo_test
        "one s turn on the cube that has made an s' turn should be equivalent default cube (i.e., the state that hasn't \
         changed)"
         (check_undo s_turn s'_turn 1000);
      undo_test "four s' turns should return the cube to its original state"  (check_undo_four_turns s'_turn 100);
  ]

let y_cube = [| blue_face; yellow_face; orange_face; white_face; green_face; red_face |] 
let y_y_cube = [| red_face; yellow_face; blue_face; white_face; orange_face; green_face |] 

let y_tests = 
  [
    turn_test "testing one y rotate on the default cube" y_cube (init_cube y_rotate 0);
      turn_test "testing two y rotates on the default cube" y_y_cube (init_cube_two_turns y_rotate y_rotate 0);
      undo_test
        "one y' rotate on the cube that has made a y rotate should be equivalent default cube (i.e., the state that hasn't \
         changed)"
         (check_undo y'_rotate y_rotate 1000);
      undo_test "four y rotates should return the cube to its original state"  (check_undo_four_turns y_rotate 100);
  ]

let y'_cube = [| green_face; yellow_face; red_face; white_face; blue_face; orange_face |] 
let y'_y'_cube = [| red_face; yellow_face; blue_face; white_face; orange_face; green_face |] 

let y'_tests = 
  [
    turn_test "testing one y' rotate on the default cube" y'_cube (init_cube y'_rotate 0);
      turn_test "testing two y' rotates on the default cube" y'_y'_cube (init_cube_two_turns y'_rotate y'_rotate 0);
      undo_test
        "one y rotate on the cube that has made a y' rotate should be equivalent default cube (i.e., the state that hasn't \
         changed)"
         (check_undo y_rotate y'_rotate 1000);
      undo_test "four y' rotates should return the cube to its original state"  (check_undo_four_turns y'_rotate 100);
  ]

let x_cube = [| yellow_face; red_face; green_face; orange_face; white_face; blue_face |] 
let x_x_cube = [| red_face; white_face; green_face; yellow_face; orange_face; blue_face |] 

let x_tests = 
  [
    turn_test "testing one x rotate on the default cube" x_cube (init_cube x_rotate 0);
      turn_test "testing two x rotates on the default cube" x_x_cube (init_cube_two_turns x_rotate x_rotate 0);
      undo_test
        "one x' rotate on the cube that has made an x rotate should be equivalent default cube (i.e., the state that hasn't \
         changed)"
         (check_undo x'_rotate x_rotate 1000);
      undo_test "four x rotates should return the cube to its original state"  (check_undo_four_turns x_rotate 100);
  ]

let x'_cube = [| white_face; orange_face; green_face; red_face; yellow_face; blue_face |] 
let x'_x'_cube = [| red_face; white_face; green_face; yellow_face; orange_face; blue_face |] 

let x'_tests = 
  [
    turn_test "testing one x' rotate on the default cube" x'_cube (init_cube x'_rotate 0);
      turn_test "testing two x' rotates on the default cube" x'_x'_cube (init_cube_two_turns x'_rotate x'_rotate 0);
      undo_test
        "one x rotate on the cube that has made an x' rotate should be equivalent default cube (i.e., the state that hasn't \
         changed)"
         (check_undo x_rotate x'_rotate 1000);
      undo_test "four x' rotates should return the cube to its original state"  (check_undo_four_turns x'_rotate 100);
  ]
 
let z_cube = [| orange_face; blue_face; yellow_face; green_face; red_face; white_face |] 
let z_z_cube = [| orange_face; white_face; blue_face; yellow_face; red_face; green_face |] 

let z_tests = 
  [
    turn_test "testing one z rotate on the default cube" z_cube (init_cube z_rotate 0);
      turn_test "testing two z rotates on the default cube" z_z_cube (init_cube_two_turns z_rotate z_rotate 0);
      undo_test
        "one z' rotate on the cube that has made a z rotate should be equivalent default cube (i.e., the state that hasn't \
         changed)"
         (check_undo z'_rotate z_rotate 1000);
      undo_test "four z rotates should return the cube to its original state"  (check_undo_four_turns z_rotate 100);
  ]

let z'_cube = [| orange_face; green_face; white_face; blue_face; red_face; yellow_face |] 
let z'_z'_cube = [| orange_face; white_face; blue_face; yellow_face; red_face; green_face |] 

let z'_tests = 
  [
    turn_test "testing one z' rotate on the default cube" z'_cube (init_cube z'_rotate 0);
      turn_test "testing two z' rotates on the default cube" z'_z'_cube (init_cube_two_turns z'_rotate z'_rotate 0);
      undo_test
        "one z rotate on the cube that has made a z' rotate should be equivalent default cube (i.e., the state that hasn't \
         changed)"
         (check_undo z_rotate z'_rotate 1000);
      undo_test "four z' rotates should return the cube to its original state"  (check_undo_four_turns z'_rotate 100);
  ]

let suite = "test suite for Cube" >::: List.flatten [ u'_tests; u_tests; f_tests; f'_tests; d_tests; d'_tests; b_tests; b'_tests;
r_tests; r'_tests; l_tests; l'_tests; m_tests; m'_tests; e_tests; e'_tests; s_tests; s'_tests; y_tests; y'_tests; x_tests; x'_tests;
z_tests; z'_tests ]
let _ = run_test_tt_main suite
