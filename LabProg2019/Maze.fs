(*
* LabProg2019 - Progetto di Programmazione a.a. 2019-20
* Maze.fs: maze
* (C) 2019 Alvise Spano' @ Universita' Ca' Foscari di Venezia
*)

module LabProg2019.Maze
open Engine
open External
open Gfx
open System

type CharInfo with
    /// Shortcut for creating a wall pixel.
    static member vertWall = pixel.create (Config.vert_wall, Color.White)
    /// Shortcut for creating a wall pixel.
    static member horizWall = pixel.create (Config.horiz_wall, Color.White)
    /// Shortcut for creating a path pixel.
    static member internal path = pixel.filled Color.Black
    ///Shortcut for creating a end of maze pixel
    static member internal end_maze= pixel.filled Color.Red
    /// Check whether this pixel is a wall.
    member this.isWall = this = pixel.vertWall
          

let pxV = pixel.vertWall      
let pxH = pixel.horizWall
let pxP = pixel.path
let pxE = pixel.end_maze

type direction = Up|Down|Right|Left|Steady

type stateM= {
    mainscreen : sprite
    player:sprite
    mutable sprites : (sprite * float * float* direction)[]
    mutable coins : (sprite * float)[]
    mutable trapano_attivato : double
}

//function that instantiates random numbers
let rnd : int -> int =    
    let gen = new System.Random()
    fun max -> gen.Next(max)

//function that chooses a random element from a list 
let chooseRandom (list:'a list) = list.[rnd list.Length]

type maze (width, height) =
    inherit image (width,height)    
    
    //creates the maze array
    //true=wall, false=path
    let mazeArray = Array2D.init width height (fun _ _ ->true) //true=wall,false=path

    //function that checks whether a point is within the maze area
    let isLegalPoint (x,y) =                       
        x >= 0 && x < width-1 && y >= 0 && y < height-1
 
    //function that checks whether the points (x-3, y) and (x, y-2) are legal points.
    //(x-3,y) is the cell in the west, (x,y-2) is the cell in the north
    //The function returns a list of legal points.
    let neighbours (x,y)=  
        let rec aux lst =
            match lst with
            |[]->[]
            |(x,y)::xs-> if isLegalPoint (x,y)
                         then (x,y)::(aux xs)
                         else aux  xs           
        in aux [(x-3,y); (x,y-2)]
    
    //function that removes walls in (x, y) and in neighboring cells (these cells are selected based on prevx)
    //The function creates a path
    let removeWallBetween (prevx,prevy) (x,y) =
        if prevx=x
        then mazeArray.[x,y] <- false
             mazeArray.[x,y-1] <- false
             mazeArray.[x+1,y] <- false
             mazeArray.[x+1,y-1] <- false
        else mazeArray.[x,y] <- false
             mazeArray.[x+1,y] <- false
             mazeArray.[x-1,y] <- false
             mazeArray.[x-2,y] <- false

    //chooses a random point between North and West.
    //To be chosen the cells must be legal points
    let chooseN_or_W (x,y)=        
        let rec aux listNeigh=
            match listNeigh with
            |[] -> (x,y)
            |[(x1,y1)] -> (x1,y1)
            |_->chooseRandom listNeigh
        aux (neighbours (x,y))
    
    //adds walls in the last row (only if the height is odd)
    let rec add_wall_lastr (x,y) num=
        if (x=width-1) then ()
        else match num with
             |n when n<=2 -> if mazeArray.[x,y] = false 
                             then mazeArray.[x,y+1] <- false
                                  mazeArray.[x+1,y+1] <- false
                                  add_wall_lastr (x+2,y) (n+1)
                             else add_wall_lastr (x+2,y) 1
             |n when n>2->let rec check (x1,y1)=
                            if mazeArray.[x1,y1] = false 
                            then mazeArray.[x1-1,y1+1] <- true
                                 mazeArray.[x1-2,y1+1] <- true
                                 mazeArray.[x1,y1+1] <- false
                                 mazeArray.[x1+1,y1+1] <- false
                                 add_wall_lastr (x1+2,y1) (n+1)
                            else add_wall_lastr (x1+2,y1) 1
                          in check (x,y)
             |_->failwith "error"

    //adds walls in the last column (only if the height is odd)
    let rec add_wall_lastc (x,y) num=
        if (y=height-1) then ()
        else match num with
             |n when n<=2->if mazeArray.[x,y] = false 
                           then mazeArray.[x+1,y] <- false
                                mazeArray.[x+2,y] <- false
                                add_wall_lastc (x,y+1) (n+1)
                           else add_wall_lastc (x,y+1) 1
             |n when n>2->let rec check (x1,y1)=
                                if mazeArray.[x1,y1]=false 
                                then mazeArray.[x1+1,y1-1] <- true
                                     mazeArray.[x1+2,y1-1] <- true
                                     mazeArray.[x1+1,y1] <- false
                                     mazeArray.[x1+2,y1] <- false
                                     add_wall_lastc (x1,y1+1) (n+1)
                                else add_wall_lastc (x1,y1+1) 1
                          in check (x,y)
             |_->failwith "error"

    //Algorithm for creating the maze (based on the binary tree algorithm).
    //This algorithm starts from point (0,0) and visits every even row.
    //In each cell of even rows the algorithm chooses a random point between the north point (x,y-2) 
    //and the west point (x-3, y) and creates a path between the current point and the chosen point . 
    //If one of the two is not a legal point, the other one is chosen. 
    //If both are not legal points the current point and the point in position (x+1,y) become path
    let rec visit (x,y)=                        //algoritmo per la creazione del maze (simile al growing tree)
        let (prevx,prevy)=chooseN_or_W (x,y)
        if (x,y)=(prevx,prevy) 
        then mazeArray.[x,y] <- false
             mazeArray.[x+1,y] <- false
             visit (x+4,y)
        else removeWallBetween (prevx,prevy) (x,y) 
             if isLegalPoint (x+4,y) then visit (x+4,y)
             else if isLegalPoint (0,y+2) then visit (0,y+2)
                       else  add_wall_lastr (0,height-3) 1//visit last row/columns to add path
                             add_wall_lastc (width-4,0) 1                 
    do visit (0,0)

    member this.generate (width,height)= 
        let draw_wall=
            for y=0 to height-1 do
                for x=0 to width-1 do
                    if y=height-2 && x=width-2 
                    then this.plot(x,y,pxE)
                         this.plot(x-1,y,pxE)
                    else if y%2=0 && y>1 
                         then if mazeArray.[x,y]=true then this.plot(x,y,pxV) 
                              else this.plot(x,y,pxP)
                         else if mazeArray.[x,y]=true then this.plot(x,y,pxH)
                              else this.plot(x,y,pxP)
        draw_wall  
 
    //returns true if (x,y) is a path and a legal point, otherwise false
    member this.isPath(x,y)=
        let isAPath= if isLegalPoint(x,y) && not (mazeArray.[x,y])  then true 
                     else false
        isAPath

    //returns true if (x,y) ia a legal point, otherwise false
    member this.legalPoint(x,y)=
        let legalPoint= if isLegalPoint(x,y) then true 
                          else false
        legalPoint

    //removes the wall (if any) in (x, y) and in (x, y + 1)
    //it is used to eliminate walls for drill mode for task3 (MazeGame): 
    //every time the drill is activated and the player is positioned above a wall, this will become a path
    member this.drillWall(x,y)=
        if mazeArray.[x,y] 
        then this.plot(x,y,pxP)
             this.plot(x-1,y,pxP)
             mazeArray.[x,y]<-false
             mazeArray.[x-1,y]<-false


let player1 (engine:engine) = engine.create_and_register_sprite (image.rectangle (2, 1, pixel.filled Color.Blue), 2 , 3, 1000)

let my_maze (width,height) = new maze (width-3,height-1)


let create_maze_spr (width, height, (engine:engine), (my_maze:maze))=
    my_maze.generate(width-3,height-1)
    let spr1 = new sprite (my_maze, 0, 0, 1)
    ignore <| engine.create_and_register_sprite (spr1, 2, 3, 1)
    ignore <| engine.create_and_register_sprite (image.rectangle (width-2, height, pixel.filled Color.White), 1 , 2, 2)

let movement_maze ((keyo:ConsoleKeyInfo option), (player1:sprite), (my_maze:maze), (st:stateM), width, height)=
    let (x,y) =
        match keyo with
        | None -> (0.,0.)  
        | Some key ->
        match key.KeyChar with 
                   | 'w' -> (0.,-1.)
                   | 'a' -> (-2.,0.)
                   | 's' -> (0.,+1.)
                   | 'd' -> (+2.,0.)
                   | _   -> (0.,0.)
    let nx = player1.x-1.0+x
    let ny = player1.y-3.0+y
    if my_maze.isPath (int(nx),int(ny)) then  player1.move_by (x,y)     
    else player1.move_by (0,0)
    if (int(nx),int(ny)) = ((width)-5,height-3) then WinMsg.main()

    st, match keyo with None -> false | Some k -> k.KeyChar = 'q'  