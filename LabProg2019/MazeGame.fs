(*
* LabProg2019 - Progetto di Programmazione a.a. 2019-20
* Maze.fs: maze
* (C) 2019 Alvise Spano' @ Universita' Ca' Foscari di Venezia
*)

module LabProg2019.MazeGame
open Engine
open Gfx
open System
open Maze
open Sprites
open External
open ASCII_elem


type CharInfo with 
    /// Shortcut for creating a coin pixel. sx=left cell, dx=right cell
    static member coin = pixel.create (Config.coin_char, Color.DarkYellow)

let img_spr = new image(2, 1)
let enemy_spr1 (img : image) = new sprite (img, 2, 1, 1)

let enemy_spr (width, height, (engine : engine), (my_maze : maze))= [|
    let max = num_sprites (width, height)
    for i = 1 to max do
        let img = image.rectangle (2, 1, rnd_px() )
        let (x,y) = coordinate_spr width height my_maze
        let spr = engine.create_and_register_sprite (img, x+2 , y+3 , i+50)
        let first_dir = randomDir (int spr.x-2, int spr.y-3) Steady my_maze width height
        let (dx,dy) = next_move (first_dir)
        yield spr, dx, dy, first_dir
    |]

let img_coins = new image(1, 1)
let coins_spr (img : image) = new sprite(img, 1, 1, 1)

let coins (width, height, (engine:engine), (my_maze:maze))= [|
    let max = (width/10) + 5
    let img1 = new image (1, 1)
    for i = 1 to (rnd_int max max+5 ) do
        let img1 = image.rectangle (1, 1, pixel.coin)
        let (x,y) = coordinate_coins width height my_maze
        let coin = engine.create_and_register_sprite (img1, x+2 , y+3 , i+2)
        yield coin, 0.
    |]


let create_mazeG_spr (width, height, (engine:engine) , (my_maze:maze))=
    my_maze.generate(width-3, height-1)
    let spr1 = new sprite (my_maze, 0, 0, 1)

    //creates maze as a sprite
    ignore <| engine.create_and_register_sprite (spr1, 2, 3, 1) 
    ignore <| engine.create_and_register_sprite (image.rectangle (width-2, height, pixel.filled Color.White), 1 , 2, 999)
    
    //creates frame of drill information
    let frame_drill = new frame_text (22,7)
    frame_drill.generate(22,7) Color.Blue
    let spr2 = new sprite (frame_drill, 0, 0, 2)
    ignore <| engine.create_and_register_sprite (spr2, width, 2, 2)

    //creates frame of coin information
    let frame_coin = new frame_text (22,8)
    frame_coin.generate(22,8) Color.Blue
    let spr2 = new sprite (frame_coin, 0, 0, 2)
    ignore <| engine.create_and_register_sprite (spr2, width, 10, 2)


let mutable drill_mode = false

//drill countdown 
let mutable countdown = double (0.0)

//cd=countdown. 0=not activated, 1=activated , 2=drill_mode activated
let mutable cd_activated = 0     

//if "ON" the drill is activated, if "OFF" the drill is deactivated (shown on the screen)
let mutable state_drill = "OFF"

//number of coins collected (shown on the screen)
let mutable coin_collected = 0

//total coins to collect
let mutable tot = 0

//variables used to move coins once collected
let mutable nx = 0
let mutable ny = 0


let movement_mazeG ((keyo:ConsoleKeyInfo option), inf, (screen:wronly_raster), (player1:sprite), (my_maze:maze), (st:stateM), width, height)= // move player1
    if cd_activated <> 1 then countdown <- inf.timer
    let move=
            match keyo with
            | None -> st.player.move_by (0,0)
            | Some key ->
                let (x,y) =
                    match key.KeyChar with 
                    | 'w' -> (0.,-1.)
                    | 'a' -> (-2.,0.)
                    | 's' -> (0.,+1.)
                    | 'd' -> (2.,0.)
                    | ' ' -> if int countdown-int inf.timer = 0 && cd_activated = 0
                             then st.trapano_attivato <- inf . timer
                                  drill_mode <- true
                                  cd_activated <- 2
                                  state_drill <- "ON"
                             (0.,0.)
                    | _   -> (0.,0.)

                let nx = player1.x-1.0 + x
                let ny = player1.y-3.0 + y
                if drill_mode = false then if my_maze.isPath (int(nx), int(ny))  then  player1.move_by (x,y)     
                                           else st.player.move_by (0,0)
                else if my_maze.legalPoint (int(nx),int(ny)) 
                        then player1.move_by (x,y)
                             my_maze.drillWall (int(nx),int(ny))   
                        else player1.move_by (0,0)
                if (int(nx),int(ny)) = (width-5,height-3) then WinMsg.main()

            if drill_mode
            then if inf.timer-st.trapano_attivato >= 5.
                 then drill_mode <- false
                      countdown <- 15.0+inf.timer    //starts the countdown from 15
                      cd_activated <- 1
                      state_drill <- "OFF"
            else st.trapano_attivato <- inf.timer-5. //keeps the drill timer at 5 when it's deactivated
            if cd_activated=1 && countdown-inf.timer <= 0.
            then cd_activated <- 0
                 
            screen.draw_text (sprintf "Drill", width+8, 3, Color.Magenta, Color.Black)
            screen.draw_text (sprintf "Time drill: %.2f" (inf.timer-st.trapano_attivato), width+3, 5, Color.White, Color.Black)

            let color_statedrill state_drill= 
                if state_drill="ON" 
                then  Color.Green
                else  Color.Red

            screen.draw_text (sprintf "Drill mode: %s" state_drill , width+3, 6, color_statedrill state_drill, Color.Black)
            screen.draw_text (sprintf "Countdown:  %.2f" (countdown-inf.timer), width+3, 7, Color.White, Color.Black)
             

    let sprites = [|
        for spr, dx, dy, prevdir in st.sprites do
            spr.move_by (dx, dy)
            //if the player's coordinates and those of an enemy sprite are the same, the loss message is issued
            if (int spr.x,int spr.y) = (int player1.x,int player1.y) || 
               (int spr.x,int spr.y) = (int player1.x+1,int player1.y) ||
               (int spr.x,int spr.y) = (int player1.x-1,int player1.y) then LoseMsg.main()
            let (cx,cy) = (int spr.x-2,int spr.y-3)  
            //returns the next directions (random)
            let nextdir = if (cx%2 = 0) 
                          then if try_dir (cx,cy) prevdir my_maze width height 
                               then randomDir (cx,cy) (opposite prevdir) my_maze width height
                               else randomDir (cx,cy) prevdir my_maze width height
                          else prevdir            
            let (dx,dy) = next_move nextdir 
            yield spr, dx, dy, nextdir
        |]
    st.sprites <- sprites

    let coins = [|
        for coin, num in st.coins do
            //if the coordinates of the player and the sprite of a coin are the same, the coin counter increases by 1
            if (int player1.x,int player1.y) = (int coin.x,int coin.y)
            then if coin_collected = 20 then ny <- 1
                                             nx <- 0
                 coin.move_by (width+1+nx - int coin.x, 15+ny - int coin.y)
                 coin_collected <- coin_collected+1
                 nx <- nx+1
            yield coin, num  
        |]
    st.coins <- coins
    
    let list_coins= coins |> Array.toList
    tot <- List.length (list_coins)

    screen.draw_text (sprintf "Coins", width+8, 11, Color.Magenta, Color.Black)

    let color_coin_collected coin tot= 
        if coin = tot 
        then  Color.Green
        else  Color.White

    screen.draw_text (sprintf "Coins collected: ", width+3, 13, Color.White, Color.Black)
    screen.draw_text (sprintf "%d" coin_collected, width+8, 14, color_coin_collected coin_collected tot, Color.Black)
    screen.draw_text (sprintf "/%d" tot, width+10, 14, Color.White, Color.Black)

    st, match keyo with None -> false | Some k -> k.KeyChar = 'q'
    
    




