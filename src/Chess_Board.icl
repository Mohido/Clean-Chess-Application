module Chess_Board

import StdEnv, StdIO, StdFunc, StdDebug ///StdFunc contains seq, StdDebug contains trace_n

///constants
TILE_SIZE :== 64 //pixels, This will be the unit size of our tiles. So every coordingate will be multiplied by it to get the precise pixel.

::PiecePicture = {
			 tileWidth::Int  , 
			 tileHeight:: Int , 
			 arrayOfPixels :: [RGBColour] 
		   }
		   
:: Piece = {
	xCord:: Int, 
	yCord :: Int,
	player:: Int, 
	sprite:: PiecePicture
	}


readPicture :: *File -> (PiecePicture,*File)
readPicture file
	#! (b1, x, file) = freadi file
	#! (b2, y, file) = freadi file
	#! (pixels, file) = getPixels file 
	| b1 && b2 = ({tileWidth = x,tileHeight = y,arrayOfPixels = pixels}, file)
	| otherwise = abort "Reading Error 1 !"

getPixels :: *File -> ([RGBColour], *File)
getPixels file
 	#(isEnd, file) = fend file
  	|isEnd = ([], file)
	# (b1,r1 ,file) = freadi file
	# (b2,r2 ,file) = freadi file
	# (b3,r3 ,file) = freadi file
	| not(b1 && b2 && b3) = ([], file) 
  	#rec = {r = r1, g = r2, b=r3}
  	#(res,file) = getPixels file
 	=([rec:res],file)
	

LoadPicture :: String *World ->  (PiecePicture,*World)
LoadPicture fname w
	#! (ok, file, w) = fopen fname FReadText w
	| not ok = abort "Can't open"
	#! (content, file) = readPicture file
	//# (ok, w) = fclose file w
	//| not ok = abort "Can't close"
	= (content, w)

instance == RGBColour
where
	(==) x y = x.r == y.r && x.b == y.b && x.g == y.g
	(==) _ _ = False

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
					  WindowClose quit, /// using the quit function defined below.
					  WindowViewSize {w = 8*TILE_SIZE, h = 8*TILE_SIZE}, /// defining the size of the window.
					  WindowLook False (paintFun piece),   /// This will take the state and update state away.
					  WindowMouse (const True) Able handlingMouseEvent /// defines a mouse event system and attach handlingMouseEvent function to it.
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
		# pic = setPenColour (RGB rgbColour) pic  // this is for setting the colour of the brush.
		= paintPiece piece {x =0 , y= 0} (seq fillingFunctions pic)	  //   Apply a set of functions on an object sequently.
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

	
	
		/// ___________ Rendering Pieces functions
		paintPiece :: PiecePicture !Point2  *Picture -> *Picture
		paintPiece pie coord pic  = paintPieceAux  pointsAndColours pic
		where 
			pointsAndColours = [ ((coord.x*TILE_SIZE + yPixel, coord.y*TILE_SIZE + xPixel)
				, getPixelValue (xPixel, yPixel) pie )
				\\ yPixel <- [0..TILE_SIZE] ,  xPixel <- [0..TILE_SIZE] | not ( getPixelValue (xPixel, yPixel) pie == {r=255, g=0, b=255}) ]
		
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



