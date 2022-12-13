(** Manages user input and visuals such as the counter, background, and buttons.*)

val darkmode : int -> unit
(** [darkmode bkgrnd] changes the button responsible for changing the background
    color to the corresponding adjacent color. That is, given a color represented 
    as a hex code [bkgrnd], [darkmode] changes the text of the button to "LIGHT" 
    if the background is in the dark mode setting, and "DARK" if in the light 
    mode setting. The width of the button is accounted for based on the word. *)

val draw_buttons : int -> unit
(** [draw_buttons color] takes in a color represented as an int and draws 
    buttons of said color on the screen. Said buttons correspond to a set of 
    actions, namely: F, U, R, B, L, D and their associated counterparts, as well 
    as a RANDOMIZE" button, a "SOLVE" button, a "3x3 and "2x2" button, a 
    "2D" and "3D" button, and a button that represents the adjacent background 
    color (either "LIGHT" or "DARK"). This function also takes into account 
    whether or not the cube is a 3x3 and its dimension, drawing the appropriate 
    cube as necessary. *)

val draw_count : string -> int ref -> unit
(** [draw_count count n] draws a counter on the screen that displays a number which 
    increments upon moves made by the user.*)

val change_view : bool -> bool -> unit
(** [change_view is3x3 is3d] is responsible for managing the current 
    representation of the cube. That is, based on what dimension the user 
    selects (2d or 3d), and what cube type (2x2 or 3x3), [change_view is3x3 is3d] 
    alters the cube depiction to match said selections. *)

val check_dark_click : int -> int -> int -> unit
(**[check_dark_click x y bkgrnd] is responsible for changing the color of the 
    background to the adjacent color present on the "LIGHT/DARK" button on screen. 
    That is, users can select a dark mode, or a light mode, resulting in 
    the background changing in response. *)

val read_key : unit
(** [read_key] parses user keyboard presses or mouse clicks and engages in 
    the corresponsing action based on said press/click. [read_key] houses all 
    actions (e.g., F turn), which map to their corresponding letter on the keyboard, 
    as well as other miscellaneous features (e.g., randomize), such that when 
    the appropriate button is pressed, the corresponding action is made. [read_key] 
    also adds functionality to the buttons on screen such that when the user's mouse 
    clicks on a given button, the changes associated with said button are made. *)
