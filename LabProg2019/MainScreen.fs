module LabProg2019.MainScreen
open Engine
open External
open Gfx
open System
open Maze
open MazeAuto
open MazeGame
open ASCII_elem

type CharInfo with 
    /// Shortcut for creating every frame 
    static member select_gm = pixel.create (Config.select_gm_char, Color.Red)
    static member gm1 = pixel.create (Config.gm_char, Color.Blue)
    static member gm2 = pixel.create (Config.gm_char, Color.Green)
    static member gm3 = pixel.create (Config.gm_char, Color.DarkYellow)

type stateM= Maze.stateM

let main ()=
    //change maze width of each gamemode (min=13 , max=75)
    let width_maze = 35

    //change maze height of each gamemode (min=15 , max=36)
    let height_maze = 30

    //check the maze width (if width>75 sets it to 75)
    //if width<13 sets it to 13
    let width = if width_maze > 75 then 75
                else if width_maze < 13 then 13
                     else width_maze

    //check the maze height (if height>36 sets it to 36)
    //if height<15 sets it to 15
    let height = if height_maze > 36 then 36
                 else if height_maze < 15 then 15
                      else height_maze
    
    // ms = Main Screen
    let ms_w=
        if (width*2+50) > 173 then 173
        else width*2+50
    let ms_h=
        if (height+29) > 38 then 38
        else height+29
    
    let engine = new engine (ms_w,ms_h)

    let text = new title_text (47, 10)
    text.generate(47, 10)
    let spr1 = new sprite (text, 0, 0, 1)
    //create ASCII title
    let mutable title_text = engine.create_and_register_sprite (spr1, ms_w/2-23, 3, 1)

    //create frame of the text "select mode"
    let mutable select_mode1 = engine.create_and_register_sprite (image.rectangle (12, 1, pixel.select_gm), ms_w/2-21 , 14, 9)
    let mutable select_mode2 = engine.create_and_register_sprite (image.rectangle (12, 1, pixel.select_gm), ms_w/2+11 , 14, 9)

    //create frame of gamemode1
    let mutable gamemode1 = engine.create_and_register_sprite (image.rectangle (42, 3, pixel.gm1), ms_w/2-20 , 18, 9)
    let mutable gamemode1_2 = engine.create_and_register_sprite (image.rectangle (44, 3, pixel.gm1), ms_w/2-21 , 18, 9)

    //create frame of gamemode2
    let mutable gamemode2 = engine.create_and_register_sprite (image.rectangle (42, 3, pixel.gm2), ms_w/2-20 , 22, 9)
    let mutable gamemode2_2 = engine.create_and_register_sprite (image.rectangle (44, 3, pixel.gm2), ms_w/2-21 , 22, 9)

    //create frame of gamemode3
    let mutable gamemode3 = engine.create_and_register_sprite (image.rectangle (42, 3, pixel.gm3), ms_w/2-20 , 26, 9)
    let mutable gamemode3_2 = engine.create_and_register_sprite (image.rectangle (44, 3, pixel.gm3), ms_w/2-21 , 26, 9)

    //0=Menu, 1=Maze, 2=Automatic maze solve, 3=Pac_man           
    let mutable select_gamemode = 0 

    let mutable maze = Maze.my_maze (width*2, height)

    let mutable mazeA = MazeAuto.my_maze (width*2, height)
    
    let mutable player1 = Maze.player1 engine
    player1.clear

    let mutable enemy_sprite = [|
        (enemy_spr1(img_spr), 0., 0., Steady)
        |]

    let mutable coin_sprite = [|
        (coins_spr(img_coins), 0.)
        |]

    let mutable drill_activated = double (0.)


    let delete_home_spr()=
        select_mode1.clear
        select_mode2.clear
        title_text.clear
        gamemode1.clear
        gamemode1_2.clear
        gamemode2.clear
        gamemode2_2.clear
        gamemode3.clear
        gamemode3_2.clear

    let update (keyo : ConsoleKeyInfo option) (screen : wronly_raster) (inf : info) (st)  =
        let gamemode =
            match keyo with
            | None -> () 
            | Some key ->
                    match key.KeyChar with
                    | '1' -> delete_home_spr()
                             player1 <- Maze.player1 engine
                             create_maze_spr (width*2, height , engine, maze)
                             select_gamemode <- 1
                             ()        
                    | '2' -> delete_home_spr()
                             player1 <- Maze.player1 engine
                             create_mazeA_spr (width*2, height, engine, mazeA)
                             select_gamemode <- 2
                             ()
                    | '3' -> delete_home_spr()
                             player1 <- Maze.player1 engine
                             create_mazeG_spr (width*2, height, engine, maze)
                             st.sprites <- enemy_spr (width*2, height, engine, maze)
                             st.coins <- coins (width*2, height, engine, maze)
                             select_gamemode <- 3
                             ()
                    |_ -> () 
             
        if select_gamemode = 1 
        then Maze.movement_maze(keyo, player1, maze, st, width*2, height)    
        else if select_gamemode = 2 
             then MazeAuto.movement_mazeA(keyo, screen, player1, mazeA, st, width*2, height) 
             else if select_gamemode = 3 
                  then MazeGame.movement_mazeG (keyo, inf, screen, player1, maze, st, width*2, height)  
                  else screen.draw_text (sprintf "SELECT A GAMEMODE" , ms_w/2-8, 14, Color.White, Color.Black)
                       screen.draw_text (sprintf "Task1: MAZE (press 1)" , ms_w/2-9, 19, Color.White, Color.Black)
                       screen.draw_text (sprintf "Task2: AUTOMATIC MAZE SOLVER (press 2) " , ms_w/2-18, 23, Color.White, Color.Black)
                       screen.draw_text (sprintf "Task3: PAC-MAN MAZE (press 3)" , ms_w/2-14, 27, Color.White, Color.Black)
                       st, match keyo with None -> false | Some k -> k.KeyChar = 'q'

    let st0 = {
        mainscreen = title_text
        player = player1
        sprites = enemy_sprite
        coins =  coin_sprite
        trapano_attivato = drill_activated
               }
                       
    engine.loop update st0

