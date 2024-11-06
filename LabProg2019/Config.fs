(*
* LabProg2019 - Progetto di Programmazione a.a. 2019-20
* Config.fs: static configuration
* (C) 2019 Alvise Spano' @ Universita' Ca' Foscari di Venezia
*)

module LabProg2019.Config

open Prelude

//Main Screen char (gm=gamemode)
let select_gm_char='\177'
let gm_char='\176'

//maze char
let filled_pixel_char = '\219'
let horiz_wall = '\219'
let vert_wall = '\219' 
let empty_pixel_char = ' '
let coin_char= '$'  //'\233'
let footPrint_char = '\176'

//win-lose text message
let slash_char='/'
let vert_char='|'
let bslash_char='\\'
let underscore_char='_'
let point_char='.'
let accent_char='`'
let bracket_sx='('
let bracket_dx=')'

//title
let title1_char='\178'
let title2_char='\177'
let title3_char='\176'
let title4_char='\220'
let title5_char='\223'

// text frame
let horiz_f= '\205'
let vert_f= '\186'
let corner1= '\201'
let corner2= '\187'
let corner3= '\200'
let corner4= '\188'

let default_flip_queue = 2  // double buffering
let default_fps_cap = 30

let log_pipe_name = "LogPipe"
let log_pipe_translate_eol = '\255'

let game_console_title = "Game Window"
let log_console_title = "Log Window"

let log_msg_color = Color.Gray
let log_warn_color = Color.Yellow
let log_error_color = Color.Red
let log_debug_color = Color.Cyan
