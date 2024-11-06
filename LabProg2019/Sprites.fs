module LabProg2019.Sprites
open Gfx
open Maze

//this module contains all sprite functions

//function that return a random number between 20 and width-6
let rec random_x (width)=
    let rnd_x = rnd_int 0 (width-6)
    if rnd_x < 20 || ((rnd_x)%2 <> 0) then random_x(width)
    else rnd_x

//function that return a random number between 10 and height-5
let rec random_y (height)=
    let rnd_y = rnd_int 0 (height-5)
    if rnd_y < 10 then random_y (height)
    else rnd_y

let rec rnd_px () =
    let col = rnd_color ()
    if col = Color.Black || col = Color.White || col = Color.Blue || col = Color.Red || col=Color.Yellow then rnd_px ()
    else pixel.filled col

//returns the maximum number of sprites based on the size of the maze
let num_sprites (width,height)= 
    let tot = width*height
    match tot with
    |n when n<300 ->rnd_int 1 2 
    |n when n<500 ->rnd_int 2 3 
    |n when n<700 ->rnd_int 2 4 
    |n when n<900 ->rnd_int 3 5 
    |n when n<1500 ->rnd_int 4 8 
    |n when n>=1500 ->rnd_int 6 10 
    |_ -> failwith "error"

//given a direction it returns the corresponding  coordinate to move the sprite
let next_move dir=
    match dir with
    |Left -> (-0.05,0.0)
    |Right -> (0.05,0.0)
    |Up -> (0.,-0.05)
    |Down -> (0.,0.05)
    |_ -> failwith "error"

let opposite dir=
    match dir with
    |Left -> Right
    |Right -> Left
    |Up -> Down
    |Down -> Up
    |_ -> failwith "error"

let rec sum_coins list=
    match list with
    |[]->0
    |(spr,n)::xs->1+(sum_coins xs)

//function that checks whether a point is within the maze area
let areaSpr (x,y) width heigth =   
    (x,y)<>(width-5,heigth-5) && (x,y)<>(width-6,heigth-5) && (x,y)<>(width-6,heigth-4)

//returns a random starting coordinate for an enemy sprite  
//the coordinates of each sprite must be positioned in the path
let rec coordinate_spr width height (my_maze:maze) = 
    let x=random_x (width)
    let y=random_y (height)
    if  my_maze.isPath(x,y) then (x,y) 
    else coordinate_spr width height my_maze

let mutable list_coin:(int*int) list=[]

//returns a random starting coordinate for a coin sprite
//the coordinates of each sprite must not be the same and must be positioned in the path 
let rec coordinate_coins width height (my_maze:maze)=
    let x=rnd_int 0 (width-6)
    let y=rnd_int 0 (height-5)
    if List.contains ((x,y)) list_coin then coordinate_coins width height my_maze
    else if  my_maze.isPath(x,y) && x%2=0 && (x<>0 || y<>0) 
         then list_coin<-(x,y)::list_coin
              (x,y) 
         else coordinate_coins width height my_maze

//function that returns true if the direction is valid, false otherwise
let rec try_dir (x,y) prevdir (my_maze:maze) width heigth= 
    match prevdir with
    |Left -> if my_maze.isPath(x-1,y) && areaSpr(x-1,y) width heigth then true
                else false
    |Right ->if my_maze.isPath(x+2,y) && areaSpr(x+2,y) width heigth then true
                else false
    |Up ->if my_maze.isPath(x,y-1) && areaSpr(x,y-1) width heigth then true
            else false
    |Down ->if my_maze.isPath(x,y+1) && areaSpr(x,y+1) width heigth then true
            else false
    |_ -> failwith "error"

//returns a random direction for the enemy sprite, making sure that the sprite does not go back (when possible)
let randomDir (x,y) prevdir (my_maze:maze) width heigth =    
    let rec rnd_Dir list_dir=
        let dir= chooseRandom list_dir
        if try_dir (x,y) dir my_maze width heigth then dir
        else rnd_Dir (List.filter((<>) dir)list_dir)
    in rnd_Dir (List.filter((<>) prevdir)[Left;Right;Up;Down])
    

    