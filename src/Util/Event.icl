implementation module Util.Event

import StdEnv, StdIO, StdDebug, Util.Constants


///____________ Mouse Handling events functions_____________
		
handlingMouseEvent :: MouseState (.ls, *PSt .l) -> (.ls,*PSt .l)
handlingMouseEvent (MouseDown hitPoint _ _) pst	
# msg = ("clicked tile: (" +++ toString (hitPoint.x / TILE_SIZE) +++ ", " +++ toString (hitPoint.y / TILE_SIZE) +++ ")")
=  trace_n msg pst
handlingMouseEvent _ pst =  pst


///____________ Other Window Handling events functions_____________
quit:: (.ls, *PSt .l) -> (.ls, *PSt .l)
quit (local, pst) = (local, closeProcess pst)
		
	
	