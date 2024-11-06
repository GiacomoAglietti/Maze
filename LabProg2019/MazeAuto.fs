(*
* LabProg2019 - Progetto di Programmazione a.a. 2019-20
* Maze.fs: maze
* (C) 2019 Alvise Spano' @ Universita' Ca' Foscari di Venezia
*)

module LabProg2019.MazeAuto
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
    ///Shortcut for creating a footprint pixel
    static member internal footPrint= pixel.create (Config.footPrint_char, Color.Cyan)
    /// Check whether this pixel is a wall.
    member this.isWall = this = pixel.vertWall
      

let pxV = pixel.vertWall      
let pxH = pixel.horizWall
let pxP = pixel.path
let pxE = pixel.end_maze

type maze (width, height) =
    inherit image (width,height)    
    
    let mazeArray = Array2D.init width height (fun _ _ ->true) //true=wall , false=path
    let wasHere = Array2D.init width height (fun _ _ ->false) //true=I've been here before, false= I haven't been here before
    let correctPath = Array2D.init width height (fun _ _ ->2) //0=not correct path, 1=correct path, 2=possible correct path(not stil visited)

    //function that instantiates random numbers
    let rnd : int -> int =      
        let gen = new System.Random()
        fun max -> gen.Next(max)
    
    //function that chooses a random element from a list 
    let chooseRandom (list:'a list) = list.[rnd list.Length]  
    
    //function that checks whether a point is within the maze area
    let isLegalPoint (x,y) =                            
        x >= 0 && x < width-1 && y >= 0 && y < height-1
 
    //function that checks whether the points (x-3, y) and (x, y-2) are legal points.
    //(x-3,y) is the cell in the west, (x,y-2) is the cell in the north
    //The function returns a list of legal points.
    let neighbours (x,y)=   
        let rec aux lst =
            match lst with
            |[] -> []
            |(x,y)::xs -> if isLegalPoint (x,y)
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
            |_ -> chooseRandom listNeigh
        aux (neighbours (x,y))
    
    //add walls in the last row (only if the height is odd)
    let rec add_wall_lastr (x,y) num=
        if (x = width-1) then ()
        else match num with
             |n when n<=2 -> if mazeArray.[x,y] = false 
                             then mazeArray.[x,y+1] <- false
                                  mazeArray.[x+1,y+1] <- false
                                  add_wall_lastr (x+2,y) (n+1)
                             else add_wall_lastr (x+2,y) 1

             |n when n>2 -> let rec check (x1,y1)=
                                if mazeArray.[x1,y1] = false 
                                then mazeArray.[x1-1,y1+1] <- true
                                     mazeArray.[x1-2,y1+1] <- true
                                     mazeArray.[x1,y1+1] <- false
                                     mazeArray.[x1+1,y1+1] <- false
                                     add_wall_lastr (x1+2,y1) (n+1)
                                else add_wall_lastr (x1+2,y1) 1
                            in check (x,y)

             |_ -> failwith "error"

    //add walls in the last column (only if the height is odd)
    let rec add_wall_lastc (x,y) num=
        if (y = height-1) then ()
        else match num with
             |n when n<=2 -> if mazeArray.[x,y] = false 
                             then mazeArray.[x+1,y] <- false
                                  mazeArray.[x+2,y] <- false
                                  add_wall_lastc (x,y+1) (n+1)
                             else add_wall_lastc (x,y+1) 1

             |n when n>2-> let rec check (x1,y1)=
                                if mazeArray.[x1,y1] = false 
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
    let rec visit (x,y)=                        
        let (prevx,prevy) = chooseN_or_W (x,y)
        if (x,y) = (prevx,prevy) 
        then mazeArray.[x,y] <- false
             mazeArray.[x+1,y] <- false
             visit (x+4,y)
        else removeWallBetween (prevx,prevy) (x,y) 
             if isLegalPoint (x+4,y) then visit (x+4,y)
             else if isLegalPoint (0,y+2) then visit (0,y+2)
                       else  add_wall_lastr (0,height-3) 1
                             add_wall_lastc (width-4,0) 1
                            
    do visit (0,0)

    //function that returns a list of points
    //These points must be legal, they must not be a wall and they must not be a wrong path
    let rec possiblePath (x0,y0)=
        let rec nextcell list=
            match list with
            |[]->[]
            |(x,y)::xs -> if isLegalPoint (x,y) && mazeArray.[x,y]=false && correctPath.[x,y]<>0
                          then (x,y)::(nextcell xs)
                          else nextcell xs
        in nextcell [(x0,y0+1); (x0+2,y0); (x0,y0-1); (x0-2,y0)]
    
    //returns true if starting from (x0, y0) reaches the maze end, otherwise false
    let find_path (x0,y0)=  
        correctPath.[x0,y0] <- 1
        correctPath.[x0+1,y0] <- 1
        let rec check_path (x,y)=
            let (nx,ny) = List.head (possiblePath(x,y))
            if (nx,ny) = (width-3,height-2) then true
            else if (nx,ny) = (x0,y0) 
                 then correctPath.[x,y] <- 0
                      correctPath.[x+1,y] <- 0
                      false 
                 else if wasHere.[nx,ny] 
                      then correctPath.[x,y] <- 0
                           correctPath.[x+1,y] <- 0
                           check_path (nx,ny)
                      else correctPath.[nx,ny] <- 1 
                           correctPath.[nx+1,ny] <- 1
                           wasHere.[nx,ny] <- true
                           check_path(nx,ny)
        in check_path (x0,y0)
    
    //find the correct path
    let find_correct_path= 
        correctPath.[0,0] <- 1
        correctPath.[1,0] <- 1
        let list_neight=possiblePath(0,0)
        let rec try_aDir list=
            match list with
            |[]->()
            |(x0,y0)::xs -> if find_path (x0,y0) = true then ()
                            else correctPath.[x0,y0] <- 0
                                 correctPath.[x0+1,y0] <- 0
                                 try_aDir xs
        in try_aDir (list_neight)
    do find_correct_path

    member this.generate (width,height)= 
        let draw_wall=
            for y=0 to height-1 do
                for x=0 to width-1 do
                    if y=height-2 && x=width-2 then this.plot(x,y,pxE)
                                                    this.plot(x-1,y,pxE)
                    else if y%2=0 && y>1 
                         then if mazeArray.[x,y]=true then this.plot(x,y,pxV) 
                              else this.plot(x,y,pxP)
                         else if mazeArray.[x,y]=true then this.plot(x,y,pxH)
                              else this.plot(x,y,pxP)
        draw_wall   
    
    //print the correct path ot reach the end
    member this.draw_footPrint(width,height)=
        let draw_footPrint=
            for y=0 to height-1 do
                for x=0 to width-1 do
                    if correctPath.[x,y]=1 then this.plot (x,y,pixel.footPrint)
        draw_footPrint


let my_maze (width,height)= new maze (width-3,height-1)

let create_mazeA_spr (width, height, (engine:engine), (my_maze:maze))=
    my_maze.generate(width-3, height-1)
    let spr1 = new sprite (my_maze, 0, 0, 1)
    ignore <| engine.create_and_register_sprite (spr1, 2, 3, 1)
    ignore <| engine.create_and_register_sprite (image.rectangle (width-2, height, pixel.filled Color.White), 1 , 2, 2)
    

let movement_mazeA ((keyo:ConsoleKeyInfo option), (screen:wronly_raster), (player1:sprite), (my_maze:maze), (st:Maze.stateM), width, height) = // move player
    match keyo with
    | None -> ()  
    | Some key ->
        match key.KeyChar with
                |'t' -> my_maze.draw_footPrint (width-3,height-1)
                        player1.move_by (width-6,height-3)
                |_ -> ()
    screen.draw_text (sprintf "Press 't' to solve the maze", width+1, 3, Color.Magenta, Color.Black)

    st, match keyo with None -> false | Some k -> k.KeyChar = 'q'         

