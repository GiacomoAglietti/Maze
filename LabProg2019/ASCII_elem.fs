module LabProg2019.ASCII_elem

open External
open Gfx
open System

type CharInfo with
    // Shortcuts to create title text pixels.
    static member title0= pixel.create (Config.filled_pixel_char, Color.DarkRed)
    static member title1= pixel.create (Config.title1_char, Color.Red)
    static member title2= pixel.create (Config.title2_char, Color.Red)
    static member title3= pixel.create (Config.title3_char, Color.Magenta)
    static member title4= pixel.create (Config.title4_char, Color.DarkRed)
    static member title5= pixel.create (Config.title5_char, Color.DarkRed)

//creates the main screen title
type title_text (width,height)=
    inherit image (width,height)

    let text = Array2D.init width height (fun _ _ ->6) // 0=' ', 1=|, 2=\ , 3=/ ,4=_ , 5=. , 6=`

    //modifica elementi dell'array per creare la scritta 'YOU WIN'
    let make_text=
        for i=0 to height-1 do
            for j=0 to width-1 do
                match i with
                |0 -> if (j=11 || j=12 || j=13 || j=17 || j=18 || j=19 || j=32 || j=33 || j=34 || j=35 || j=36 || j=37 || j=38 || j=41 || j=42 || j=43 || j=44 || j=45)
                      then text.[j,i]<-0
                      if (j=20 || j=40)
                      then text.[j,i]<-1
                      if (j=31 || j=39)
                      then text.[j,i]<-2
                      if (j=1 || j=2 || j=3 || j=14 || j=16 || j=22 || j=23 || j=24)
                      then text.[j,i]<-4
                |1 -> if (j=1 || j=2 || j=3 || j=4 || j=11 || j=12 || j=15 || j=18 || j=19 || j=22 || j=23 || j=24 || j=25 || j=41)
                      then text.[j,i]<-0
                      if (j=10 || j=40)
                      then text.[j,i]<-1
                      if (j=0 || j=13 || j=20 || j=21 || j=31 || j=33 || j=35)
                      then text.[j,i]<-2
                      if (j=26 || j=39)
                      then text.[j,i]<-3
                      if (j=5 || j=26 || j=37)
                      then text.[j,i]<-4
                      if (j=14 || j=16 || j=38 || j=45)
                      then text.[j,i]<-5
                |2 -> if (j=1 || j=2 || j=6 || j=11 || j=12 || j=18 || j=19 || j=22 || j=23 || j=27 || j=41 || j=42 || j=43)
                      then text.[j,i]<-0
                      if (j=10 || j=17)
                      then text.[j,i]<-1
                      if (j=0 || j=21 || j=33 || j=36 || j=40)
                      then text.[j,i]<-2
                      if (j=20 || j=31 || j=38)
                      then text.[j,i]<-3
                      if (j=7 || j=28 || j=35 )
                      then text.[j,i]<-4
                      if (j=5 || j=26 || j=36 )
                      then text.[j,i]<-5
                |3 -> if (j=1 || j=2 || j=7 || j=8 || j=11 || j=12 || j=18 || j=19 || j=22 || j=23 || j=28 || j=29 || j=42)
                      then text.[j,i]<-0
                      if (j=41)
                      then text.[j,i]<-1
                      if (j=10 || j=17 || j=35 || j=40)
                      then text.[j,i]<-2
                      if (j=0 || j=33)
                      then text.[j,i]<-3
                      if (j=3 || j=4 || j=5 || j=6 || j=24 || j=25 || j=26 || j=27 || j=33 || j=45)
                      then text.[j,i]<-4
                      if (j=34)
                      then text.[j,i]<-5
                |4 -> if (j=2 || j=7 || j=8 || j=11 || j=12 || j=18 || j=19 || j=23 || j=28 || j=29 || j=32 || 
                          j=33 || j=34 || j=35 || j=36 || j=37 || j=38 || j=42 || j=43 || j=44 || j=45)
                      then text.[j,i]<-0
                      if (j=1 || j=6 || j=22 || j=27)
                      then text.[j,i]<-1
                      if (j=9 || j=10 || j=20 || j=30 || j=31 || j=39 || j=41 || j=46)
                      then text.[j,i]<-2
                      if (j=17 || j=40)
                      then text.[j,i]<-3
                |5 -> if (j=8 || j=29)
                      then text.[j,i]<-0
                      if (j=6 || j=27 || j=35)
                      then text.[j,i]<-1
                      if (j=1 || j=2 || j=7 || j=12 || j=22 || j=23 || j=28 || j=32 || j=33 || j=37 || j=39 || j=43)
                      then text.[j,i]<-2
                      if (j=9 || j=10 || j=13 || j=17 || j=20 || j=30 || j=31 || j=36 || j=38 || j=40 || j=41 || j=44 || j=46)
                      then text.[j,i]<-3
                |6 -> if (j=2 || j=6 || j=7 || j=23 || j=27 || j=28 || j=33 || j=35 || j=39)
                      then text.[j,i]<-2
                      if (j=10 || j=11 || j=13 || j=20 || j=30 || j=31 || j=32 || j=37 || j=41 || j=43 || j=46)
                      then text.[j,i]<-3
                |7 -> if (j=6 || j=27)
                      then text.[j,i]<-2
                      if (j=2 || j=10 || j=17 || j=23 || j=31 || j=33 || j=35 || j=37 || j=39 || j=43)
                      then text.[j,i]<-3
                |8 -> if (j=6 || j=9 || j=17 || j=27 || j=30 || j=33 || j=35 || j=43 || j=46)
                      then text.[j,i]<-3
                |9 -> if (j=30)
                      then text.[j,i]<-3
                |_ -> failwith "error"
               
    member this.generate (width,height)= 
        let draw_text=
            for y=0 to height-1 do
                for x=0 to width-1 do
                    match text.[x,y] with
                    |6 -> this.plot(x,y,pixel.empty)
                    |0 -> this.plot(x,y,pixel.title0)
                    |1 -> this.plot(x,y,pixel.title1)
                    |2 -> this.plot(x,y,pixel.title2)
                    |3 -> this.plot(x,y,pixel.title3)
                    |4 -> this.plot(x,y,pixel.title4)
                    |5 -> this.plot(x,y,pixel.title5)
                    |_ -> failwith "error"
        draw_text

//creates a frame width*height
type frame_text (width,height)=
    inherit image (width,height)

    let text = Array2D.init width height (fun _ _ ->0)

    let make_text=
        for i = 0 to height-1 do
            for j=0 to width-1 do
                match i with
                |i when i=0 -> if (j=0) then text.[j,i]<-4
                               else if (j=width-1) then text.[j,i]<-2
                                    else text.[j,i]<-6
                |i when i>0 -> if i<height-1 
                               then if (j=0 || j=width-1)  then text.[j,i]<-1
                               else if (j=0) then text.[j,i]<-3
                                    else if (j=width-1) then text.[j,i]<-5
                                         else text.[j,i]<-6
                |_ -> failwith "error"
    
    member this.generate (width,height) (color:ConsoleColor)= 
        let draw_text=
            for y=0 to height-1 do
                for x=0 to width-1 do
                    match text.[x,y] with
                    |0 -> this.plot(x,y,pixel.empty)
                    |1 -> this.plot(x,y,pixel.create (Config.vert_f, color))
                    |2 -> this.plot(x,y,pixel.create (Config.corner2, color))
                    |3 -> this.plot(x,y,pixel.create (Config.corner3, color))
                    |4 -> this.plot(x,y,pixel.create (Config.corner1, color))
                    |5 -> this.plot(x,y,pixel.create (Config.corner4, color))
                    |6 -> this.plot(x,y,pixel.create (Config.horiz_f, color))
                    |_ -> failwith "error"
        draw_text