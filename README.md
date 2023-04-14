# Oasis Cube: A Rubik's Cube Simulator in OCaml
Created by Osman Osman, Frank Zhou, Asim Bacchus

## A short demo


https://user-images.githubusercontent.com/62918647/208262921-e4d05f2e-fbe1-4567-8d14-0355d94a71b0.mov




# **Installing OCaml Graphics**
## *Mac OS X*
1. Install [XQuartz](https://www.xquartz.org/)
2. Restart system
3. Re-install all packages you have previously installed via opam. Execute these commands:
```
opam switch
opam switch cs3110-2022fa
```
3. Next, execute:
```
opam update; opam upgrade
opam install graphics
```
5. You may need to install core or batteries via 
```
opam install core
opam install batteries
```

## *Linux*
1. Make sure that x11 window manager is installed
2. Continue to Step number 3 for *Mac OS X*

## *Windows*
1. Download the terminal [MobaXterm Home Edition](https://mobaxterm.mobatek.net/download.html)
2. Proceed to Steps 2 through 5 of the *Mac OS X* setup
3. Use the terminal *within MobaXterm* to run this project

# **Running the Cube**
Execute the following lines:
```
dune utop
```
## **Controls**
### Note: Keyboard presses are case-sensitive 
- Press the "q" key to terminate the program. 
- Click the SOLVE button or press the "." key to solve the cube. 
- Click the RANDOMIZE button or press the "\" key to randomize the cube. 
- Press the "1" key to do one random turn on the cube. 
- Click the DARK button or press the "," key to turn on dark mode. 
- Click the LIGHT button or press the "<" key to turn on light mode. 
- Click the 2x2 button to change the dimensions of the cube to a 2x2 Rubik's cube. 
- Click the 3x3 button to change the dimensions of the cube to a 3x3 Rubik's cube. 
- Click the 2D button to change the view of the cube to a 2D representation. 
- Click the 3D button to change the view of the cube to a 3D representation. 
- Press the "2" key to view the 2D representation of the 2x2 cube. 
- Press the "3" key to view the 2D representation of the 3x3 cube. 
- Press the "4" key to view the 3D representation of the 2x2 Rubik's cube. 
- Press the "5" key to view the 3D representation of the 3x3 Rubik's cube. 

### Shared 2x2 and 3x3 Controls 
- Click the F button or press the "f" key to do an F turn on the cube. 
- Click the F' button or press the "F" key to do an F' turn on the cube.
- Click the R button or press the "r" key to do an R turn on the cube. 
- Click the R' button or press the "R" key to do an R' turn on the cube. 
- Click the U button or press the "u" key to do a U turn on the cube. 
- Click the U' button or press the "U" key to do a U' turn on the cube. 
- Click the B button or press the "b" key to do a B turn on the cube. 
- Click the B' button or press the "B" key to do a B' turn on the cube. 
- Click the L button or press the "l" key to do an L turn on the cube. 
- Click the L' button or press the "L" key to do an L' turn on the cube. 
- Click the D button or press the "d" key to do a D turn on the cube. 
- Click the D' button or press the "D" key to do a D' turn on the cube. 
- Press the "x" key to do an x rotation on the cube. 
- Press the "X" key to do an x' rotation on the cube. 
- Press the "y" key to do a y rotation on the cube. 
- Press the "Y" key to do a y' rotation on the cube. 
- Press the "z" key to do a z rotation on the cube. 
- Press the "Z" key to do a z' rotation on the cube. 

### Unique 3x3 Controls 
- Press the "m" key to do an M turn on the cube.
- Press the "M" key to do an M' turn on the cube. 
- Press the "e" key to do an E turn on the cube.
- Press the "E" key to do an E' turn on the cube.
- Press the "s" key to do an S turn on the cube. 
- Press the "S" key to do an S' turn on the cube. 
