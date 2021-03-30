module Chess_Board

import StdEnv, StdIO, StdFunc ///StdFunc contains seq, StdDebug contains trace_n
import Util.Reading, Util.Event, Util.Constants

:: Piece = {
	xCord:: Int, 
	yCord :: Int,
	player:: Int, 
	sprite:: PiecePicture
	}



::SetState = {
			 p :: PiecePicture
		   }
		   

Start:: *World -> *World
Start world 
#! (piece, world) = LoadPicture "white_queen.cimg" world
= startIO SDI {p = piece} (initProcessState piece) [ProcessClose closeProcess] world
where 
	initProcessState piece pst /// initialization of the interface program.
	# (windowid , pst) = openId pst
	# (errorM, pst) = openWindow undef (window windowid piece) pst
	= pst
	where
		/// _____________ Elements Gui initialization Area_____________
		window windowid piece = Window "Title" NilLS
					[	
					  WindowId windowid,
					  WindowClose quit, 									/// using the quit function defined below.
					  WindowViewSize {w = 8*TILE_SIZE, h = 8*TILE_SIZE}, 	/// defining the size of the window.
					  WindowLook False (paintFun piece),   					/// This will take the state and update state away.
					  WindowMouse (const True) Able handlingMouseEvent 		/// defines a mouse event system and attach handlingMouseEvent function to it.
					  ]
		
		///__________ Window painting functions __________________
		/**
			SelectState = Able | Unable  // if window is active or not.
			UpdateState : see it in the library.. it contains the places needs rendering.
			*Picture is passed when the funciton is called by the window attirbute. Just edit it and return a picture at the end.. drawAt and fillAt are functions that work on any drawable object. other funcitons are in the library.
			Note: This function is called whenever a refresh to the page is needed or we can make the window call it explicitly from somewhere else.
		*/
		paintFun :: PiecePicture SelectState UpdateState *Picture -> *Picture  //style 2 more suffecient.
		paintFun piece _ _ pic
		# rgbColour = {r =130, g=63, b=59} 
		# pic = setPenColour (RGB rgbColour) pic // this is for setting the colour of the brush.
		= seq ([paintPiece piece {x =xp , y= yp} \\ xp<-[0..7], yp<-[0,1,6,7]]) (seq fillingFunctions pic)	  //   Apply a set of functions on an object sequently.
		where
			tile = {box_w = TILE_SIZE, box_h = TILE_SIZE} 	/// eventually a box.
			fillingFunctions = [fillAt {x=ycord*TILE_SIZE, y=xcord*TILE_SIZE} tile \\ xcord <- [0..7] , ycord <- [0..7]|(xcord rem 2 == 0 && ycord rem 2 == 0) || (xcord rem 2 <>  0 && ycord rem 2 <> 0)]
		
		
		/// ___________ Rendering Pieces functions
		
		paintPiece :: PiecePicture !Point2  *Picture -> *Picture
		paintPiece pie coord pic  = paintPieceAux  (pointsAndColours) pic //To try the tail recursive way, replace with (pointsAndColours 0 0 [])
		where 
			
			pointsAndColours = [ ((coord.x*TILE_SIZE + yPixel, coord.y*TILE_SIZE + xPixel)
				, getPixelValue (xPixel, yPixel) pie )
				\\ yPixel <- [0..TILE_SIZE] ,  xPixel <- [0..TILE_SIZE] | not ( getPixelValue (xPixel, yPixel) pie == {r=255, g=0, b=255}) ]
			
			//Tail Recursion doesn't really make a differnece preformance wise, approach is still very slow. 
			
			//Tail Recursive:
			/*
			pointsAndColours :: Int Int [((Int ,Int) , RGBColour)] -> [((Int ,Int) , RGBColour)] 
			pointsAndColours _  TILE_SIZE acc = acc
			pointsAndColours TILE_SIZE y acc = pointsAndColours 0 (y+1) acc  
			pointsAndColours xPixel yPixel acc 
			| (myColour == {r=255, g=0, b=255}) = pointsAndColours (xPixel + 1) yPixel acc
			= pointsAndColours (xPixel + 1) yPixel (acc++[((coord.x*TILE_SIZE + yPixel, coord.y*TILE_SIZE + xPixel),myColour)]) 
				where
				myColour = getPixelValue (xPixel,yPixel) pie
			*/
			paintPieceAux :: [((Int ,Int) , RGBColour )] *Picture -> *Picture
			paintPieceAux [] pic = pic
			paintPieceAux [( (x,y) , rgb ) : rest ] pic
			# pic = setPenColour (RGB rgb) pic 
			= paintPieceAux rest (drawPointAt {x = x, y = y} pic)
		
		
		getPixelValue ::  (Int, Int) PiecePicture -> RGBColour
		getPixelValue (x,y) piece
		# xRatio = TILE_SIZE / piece.tileWidth
		# yRatio = TILE_SIZE / piece.tileHeight
		# index =  (x/xRatio) + (y/yRatio) * piece.tileWidth
		| index >= length piece.arrayOfPixels = {r=0, g=0, b=0}
		= piece.arrayOfPixels!!index
