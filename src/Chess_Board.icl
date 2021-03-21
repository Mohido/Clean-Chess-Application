module Chess_Board

import StdEnv, StdIO, StdFunc, StdDebug ///StdFunc contains seq, StdDebug contains trace_n


///constants
TILE_SIZE :== 64 //pixels, This will be the unit size of our tiles. So every coordingate will be multiplied by it to get the precise pixel.



Start:: *World -> *World
Start world = startIO SDI Void initProcessState [ProcessClose closeProcess] world
where 

	initProcessState pst /// initialization of the interface program.
	# (errorM, pst) = openWindow undef window pst
	= pst
	where
	
		/// _____________ Elements Gui initialization Area_____________
		window = Window "Title" NilLS 
					[ WindowClose quit, /// using the quit function defined below.
					  WindowViewSize {w = 8*TILE_SIZE, h = 8*TILE_SIZE}, /// defining the size of the window.
					  WindowLook True paintFun,   /// This will take the state and update state away.
					  WindowMouse (const True) Able handlingMouseEvent /// defines a mouse event system and attach handlingMouseEvent function to it.
					  ]
		
		
		
		///__________ Window painting functions __________________
		/**
			SelectState = Able | Unable  // if window is active or not.
			UpdateState : see it in the library.. it contains the places needs rendering.
			*Picture is passed when the funciton is called by the window attirbute. Just edit it and return a picture at the end.. drawAt and fillAt are functions that work on any drawable object. other funcitons are in the library.
			Note: This function is called whenever a refresh to the page is needed or we can make the window call it explicitly from somewhere else.
		*/
		paintFun :: SelectState UpdateState *Picture -> *Picture  //style 2 more suffecient.
		paintFun _ _ pic 
		# rgbColour = {r =130, g=63, b=59}
		# pic = setPenColour (RGB rgbColour) pic  // this is for setting the colour of the brush.
		= seq fillingFunctions pic 				  // Apply a set of functions on an object sequently.
		where
			tile = {box_w = TILE_SIZE, box_h = TILE_SIZE} 	/// eventually a box.
			fillingFunctions = [ fillAt {x=ycord*TILE_SIZE, y=xcord*TILE_SIZE} tile  ///List oof functions where we are supposed to fill a tile. 
									\\ xcord <- [0..7] , ycord <- [0..7] 
									| (xcord rem 2 == 0 && ycord rem 2 == 0) || (xcord rem 2 <>  0 && ycord rem 2 <> 0)]
			
		
		
		///____________ Mouse Handling events functions_____________
		
		handlingMouseEvent :: MouseState (.ls, *PSt .l) -> (.ls,*PSt .l)
		handlingMouseEvent (MouseDown hitPoint _ _) pst	
			# msg = ("clicked tile: (" +++ toString (hitPoint.x / TILE_SIZE) +++ ", " +++ toString (hitPoint.y / TILE_SIZE) +++ ")")
			=  trace_n msg pst
		handlingMouseEvent _ pst =  pst
		
		
		///____________ Other Window Handling events functions_____________
		
		quit:: (.ls, *PSt .l) -> (.ls, *PSt .l)
		quit (local, pst) = (local, closeProcess pst)










