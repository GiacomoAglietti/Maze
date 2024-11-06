module LabProg2019.WinMsg
open Engine
open External
open Gfx
open System
open ASCII_elem

type CharInfo with
    // Shortcuts to create win text pixels.
    static member vert= pixel.create (Config.vert_char, Color.White)
    static member slash= pixel.create (Config.slash_char, Color.White)
    static member bslash= pixel.create (Config.bslash_char, Color.White)
    static member underscore= pixel.create (Config.underscore_char, Color.White)
    static member empty= pixel.create (Config.empty_pixel_char, Color.White)
    static member point= pixel.create (Config.point_char, Color.White)
    static member accent= pixel.create (Config.accent_char, Color.White)

    // Shortcuts to create title text pixels.
    static member title0= pixel.create (Config.filled_pixel_char, Color.DarkRed)
    static member title1= pixel.create (Config.title1_char, Color.Red)
    static member title2= pixel.create (Config.title2_char, Color.Red)
    static member title3= pixel.create (Config.title3_char, Color.Magenta)
    static member title4= pixel.create (Config.title4_char, Color.DarkRed)
    static member title5= pixel.create (Config.title5_char, Color.DarkRed)


type state= {
    win : sprite
}

type win_text (width,height)=
    inherit image (width,height)

    let text = Array2D.init width height (fun _ _ ->0) // 0=' ', 1=|, 2=\ , 3=/ ,4=_ , 5=. , 6=`

    //modify elements of the array to create the writing 'YOU WIN'
    let make_text=
        for i=0 to height-1 do
            for j=0 to width-1 do
                match i with
                |0 -> if (j=0 || j=1 || j=7 || j=8 || j=9 || j=10 || j=11 || j=12 || j=15 || j=20 || j=23 || j=24 || j=35 || j=36 || j=37 || j=38 || j=39 || j=40 || j=41 || j=43 || j=47)
                      then text.[j,i]<-4
                |1 -> if (j=0 || j=2 || j=13 || j=23 || j=25 || j=44)
                      then text.[j,i]<-2
                      if (j=6 || j=8 || j=34 || j=36)
                      then text.[j,i]<-3
                      if (j=14 || j=16 || j=19 || j=21 || j=42 || j=46 || j=48)
                      then text.[j,i]<-1
                      if (j=10 || j=11 || j=37 || j=41)
                      then text.[j,i]<-4
                |2 -> if (j=1 || j=3 || j=24 || j=26 || j=30 || j=45)
                      then text.[j,i]<-2
                      if (j=5 || j=7 || j=29 || j=33 || j=35)
                      then text.[j,i]<-3
                      if (j=9 || j=12 || j=14 || j=16 || j=19 || j=21 || j=38 || j=40 || j=42 || j=46 || j=48)
                      then text.[j,i]<-1
                      if (j=4)
                      then text.[j,i]<-4
                |3 -> if (j=2 || j=25 || j=27 || j=31)
                      then text.[j,i]<-2
                      if (j=6 || j=28 || j=32 || j=34)
                      then text.[j,i]<-3
                      if (j=7 || j=9 || j=12 || j=14 || j=16 || j=19 || j=21 || j=38 || j=40 || j=42 || j=48 )
                      then text.[j,i]<-1
                      if (j=44)
                      then text.[j,i]<-5
                      if (j=46)
                      then text.[j,i]<-6
                |4 -> if (j=26 || j=30 || j=45)
                      then text.[j,i]<-2
                      if (j=29 || j=33)
                      then text.[j,i]<-3
                      if (j=3 || j=5 || j=7 || j=9 || j=12 || j=14 || j=16 || j=19 || j=21 || j=38 || j=40 || j=42 || j=44 || j=48 )
                      then text.[j,i]<-1
                      if (j=10 || j=11 || j=17 || j=18 || j=37 || j=41)
                      then text.[j,i]<-4
                |5 -> if (j=8 || j=15 || j=27 || j=31 || j=46)
                      then text.[j,i]<-2
                      if (j=13 || j=20 || j=28 || j=32)
                      then text.[j,i]<-3
                      if (j=3 || j=5 || j=36 || j=42 || j=44 || j=48 )
                      then text.[j,i]<-1
                      if (j=4 || j=9 || j=10 || j=11 || j=12 || j=16 || j=17 || j=18 || j=19 || j=37 || j=38 || j=39 || j=40 || j=41 || j=43 || j=47 )
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
                    |5 -> this.plot(x,y,pixel.point)
                    |6 -> this.plot(x,y,pixel.accent)
                    |_ -> failwith "error"
        draw_text






let main ()=
    let win_w=81
    let win_h=30

    //create the victory message
    let engine = new engine (win_w,win_h)
    let text = new win_text (49,6)
    text.generate(49,6)
    let spr1 = new sprite (text,0,0,1)
    ignore <| engine.create_and_register_sprite (spr1,16,11,1)

    //create the frame of the victory message
    let frame_text= new frame_text (57,11)
    frame_text.generate(57,11) Color.Yellow
    let spr2 = new sprite (frame_text,0,0,2)
    ignore <| engine.create_and_register_sprite (spr2,12,9,2)
   
    let update (keyo : ConsoleKeyInfo option) (screen : wronly_raster) (inf : info) (st : state)  = // move player1

        // calculate next state
        st, match keyo with None -> false | Some k -> k.KeyChar = 'q'      
    
    let st0 = {
        win=spr1
               }

    engine.loop update st0
    