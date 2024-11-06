module LabProg2019.LoseMsg
open Engine
open External
open Gfx
open System
open ASCII_elem

type CharInfo with
    // Shortcuts to create text pixels.
    static member vert= pixel.create (Config.vert_char, Color.White)
    static member slash= pixel.create (Config.slash_char, Color.White)
    static member bslash= pixel.create (Config.bslash_char, Color.White)
    static member underscore= pixel.create (Config.underscore_char, Color.White)
    static member empty= pixel.create (Config.empty_pixel_char, Color.White)
    static member bracket_sx= pixel.create (Config.bracket_sx, Color.White)
    static member bracket_dx= pixel.create (Config.bracket_dx, Color.White)




type state= {
    lose : sprite
}

type lose_text (width,height)=
    inherit image (width,height)

    let text = Array2D.init width height (fun _ _ ->0) // 0=' ', 1=|, 2=\ , 3=/ ,4=_ , 5=( , 6=)

    //modify elements of the array to create the writing 'YOU LOSE'
    let make_text=
        for i=0 to height-1 do
            for j=0 to width-1 do
                match i with
                |0 -> if (j=0 || j=1 || j=7 || j=8 || j=9 || j=10 || j=11 || j=12 || j=15 || j=20 || j=24 || j=31 || j=32 || 
                          j=33 || j=34 || j=38 || j=39 || j=40 || j=41 || j=42 || j=44 || j=45 || j=46 || j=47 || j=48 || j=49)
                      then text.[j,i]<-4
                |1 -> if (j=0 || j=2 || j=13 || j=35 )
                      then text.[j,i]<-2
                      if (j=6 || j=8 || j=30 || j=37)
                      then text.[j,i]<-3
                      if (j=14 || j=16 || j=19 || j=21 || j=23 || j=25 || j=43 || j=50)
                      then text.[j,i]<-1
                      if (j=10 || j=11 || j=32 || j=33 || j=39 || j=40 || j=41 || j=42 || j=46 || j=47 || j=48 || j=49)
                      then text.[j,i]<-4
                |2 -> if (j=1 || j=3 )
                      then text.[j,i]<-2
                      if (j=5 || j=7 )
                      then text.[j,i]<-3
                      if (j=9 || j=12 || j=14 || j=16 || j=19 || j=21 || j=23 || j=25 || j=29 || j=31 || j=34 || j=36 || 
                          j=43 || j=45)
                      then text.[j,i]<-1
                      if (j=4 || j=39 || j=40 || j=41 || j=46 || j=47)
                      then text.[j,i]<-4
                      if (j=38)
                      then text.[j,i]<-5
                |3 -> if (j=2 || j=37 || j=42)
                      then text.[j,i]<-2
                      if (j=6 )
                      then text.[j,i]<-3
                      if (j=7 || j=9 || j=12 || j=14 || j=16 || j=19 || j=21 || j=23 || j=25 || j=29 || j=31 || j=34 || 
                          j=36 || j=43 || j=48)
                      then text.[j,i]<-1
                      if (j=38 || j= 39 || j=40 || j=46 || j=47)
                      then text.[j,i]<-4
                |4 -> if (j=3 || j=5 || j=7 || j=9 || j=12 || j=14 || j=16 || j=19 || j=21 || j=23 || j=25 || j=29 || 
                          j=31 || j=34 || j=36 || j=43 || j=45)
                      then text.[j,i]<-1
                      if (j=10 || j=11 || j=17 || j=18 || j=26 || j=27 || j=28 || j=32 || j=33 || j=37 || j=38 || j=39 ||
                          j=40 || j=46 || j=47 || j=48 || j=49)
                      then text.[j,i]<-4
                      if (j=41)
                      then text.[j,i]<-6
                |5 -> if (j=8 || j=15 || j=30 || j=41)
                      then text.[j,i]<-2
                      if (j=13 || j=20 || j=35 || j=42)
                      then text.[j,i]<-3
                      if (j=3 || j=5 || j=23 || j=36 || j=43 || j=50)
                      then text.[j,i]<-1
                      if (j=4 || j=9 || j=10 || j=11 || j=12 || j=16 || j=17 || j=18 || j=19 || j=24 || j=25 || j=26 ||
                          j=27 || j=28 || j=29 || j=31 || j=32 || j=33 || j=34 || j=37|| j=38 || j=39|| j=40 || j=41 || 
                          j=44 || j=45 || j=46 || j=47 || j=48 || j=49 )
                      then text.[j,i]<-4
                |_ -> failwith "error"

    member this.generate (width,height)= 
        let draw_text=
            for y=0 to height-1 do
                for x=0 to width-1 do
                    match text.[x,y] with
                    |0 -> this.plot(x,y,pixel.empty)
                    |1 -> this.plot(x,y,pixel.vert)
                    |2 -> this.plot(x,y,pixel.bslash)
                    |3 -> this.plot(x,y,pixel.slash)
                    |4 -> this.plot(x,y,pixel.underscore)
                    |5 -> this.plot(x,y,pixel.bracket_sx)
                    |6 -> this.plot(x,y,pixel.bracket_dx)
                    |_ -> failwith "error"
        draw_text


let main ()=
    let win_w=81
    let win_h=30

    //create the loss message
    let engine = new engine (win_w,win_h)
    let text = new lose_text (55,6)
    text.generate(55,6)
    let spr1 = new sprite (text,0,0,1)
    ignore <| engine.create_and_register_sprite (spr1,15,11,1)

    //create the frame of the loss message
    let frame_text= new frame_text (60,9)
    frame_text.generate(60,9) Color.Yellow
    let spr2 = new sprite (frame_text,0,0,2)
    ignore <| engine.create_and_register_sprite (spr2,11,10,2)

    let update (keyo : ConsoleKeyInfo option) (screen : wronly_raster) (inf : info) (st : state)  = // move player1
        // calculate next state
        st, match keyo with None -> false | Some k -> k.KeyChar = 'q'      
    
    let st0 = {
        lose=spr1
               }

    engine.loop update st0

