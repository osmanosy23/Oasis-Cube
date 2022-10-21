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
#use "src/cube.ml";;
```
## **Controls**
- Press the "r" key to turn the cube face clockwise
- Press the "c" key to turn the cube face counter-clockwise
- Press the "q" key to terminate the program