module Chess_Board

import StdEnv, StdIO, StdFunc, StdDebug ///StdFunc contains seq, StdDebug contains trace_n
import Util.Reading, Util.Event, Util.Constants, Util.CostumFunctions


//Start Function and Initializing game assets
Start:: *World -> *World
Start world 
# gs = {worldMatrix = board, selectedPiece = Nothing, windowId = wid}  		///initial game state (process state)
= startIO SDI gs (initIO (wid, board) ) [ProcessClose closeProcess] worldFinal
where
	/*_______ world function + reading sprites________*/
	(wid ,world1 ) = openId world
	(wq_sprite, worldFinal) = LoadPicture "white_queen.cimg" world1
	
	
	/*_____________Loading pieces Area________________*/ 
	wq_piece = { xCord = 4, yCord = 7,  player = WhitePiece, type = Queen, sprite = wq_sprite} 
	
	// main board, with 8*8 pieces or nothing.
	board :: !Board
	board = {Just wq_piece}

//_____________________________________________________________________________



//____________ Initializing the window area

initIO :: (!Id, {!(Maybe Piece)}) (PSt GameState) -> (PSt GameState)
initIO w_inits=:(wid, preBoard) pst
# (errorM, pst) = openWindow undef (window) pst
= pst
where
	window = Window "Title" NilLS
								[	
									WindowId wid,
									WindowClose quit, 
									WindowViewSize {w = 8*TILE_SIZE, h = 8*TILE_SIZE}, 	/// defining the size of the window.
									WindowLook False (look preBoard) 				/// This will take the state and update state away.
								]


/*________Rendering Function_____*/

/**
* Painting the window's context. Once it critically needs updating (on creation and resizing)
*/
look :: !Board SelectState UpdateState *Picture -> *Picture
look board _ {oldFrame, newFrame} pic
# newFrameSize	= rectangleSize newFrame 				/// The new Window rectangle size
# oldFrameSize 	= rectangleSize oldFrame 				/// The old Window rectangle size
| newFrameSize.w > 8*TILE_SIZE || 	
		newFrameSize.h > 8*TILE_SIZE  = pic 			///If window resizing is not impacting the main area of the game don't redraw the whole game.
# b_col_1 = RGB {r =130, g=63, b=59} 					///Board first colour
# b_col_2 = White										///Board sec.. colour
= fillPieces board (fillBoard b_col_1 (clear b_col_2 pic))


/**
* clear the whole picture with the given colour
*/
clear :: Colour *Picture -> *Picture
clear White pic = unfill pictDomain pic
clear c_col pic
# pic = setPenColour c_col pic
= fill pictDomain pic	 				///pictDomain in Util.Constants



/**
* Fill the whole board as fast as possible.
*/
fillBoard :: Colour *Picture -> *Picture
fillBoard c_col pic 
# pic = setPenColour c_col pic
= foldr fillTile pic pix_cord_list
where
	pix_cord_list = [ {x=ycord*TILE_SIZE, y=xcord*TILE_SIZE} \\ xcord <- [0..7] , ycord <- [0..7] 
						| 
					(xcord rem 2 == 0 && ycord rem 2 == 0) || (xcord rem 2 <>  0 && ycord rem 2 <> 0)]
	
	/*Op*/
	fillTile :: !Point2 -> *Picture -> *Picture
	fillTile pix_cord = fillAt pix_cord tile


/**
* fill the board with pieces.
*/
fillPieces :: !Board *Picture -> *Picture
fillPieces board pic = fillPiecesAux board pic 0
where
	fillPiecesAux :: !Board *Picture !Int -> *Picture
	fillPiecesAux board pic ind
	| ind >= size (board) = pic
	#! pic = renderPiece board.[ind] pic
	= fillPiecesAux board pic (ind+1)



/**
* Renders a specific piece.
*/
renderPiece :: !(Maybe Piece) *Picture -> *Picture
renderPiece Nothing pic = pic
renderPiece (Just piece) pic 
# 
= pic









/**
For further game Optimisation plans:-
*	fillBoardAt :: !Rectangle !Colour *Picture -> *Picture
*	clearAt :: !Rectangle !Colour *Picture -> *Picture
*	paintPiecesAt :: !Rectangle !Board *Picture -> *Picture
*/




	/*initProcessState windState initSprites pst /// initialization of the interface program.
	# (errorM, pst) = openWindow windState window pst
	= pst
	where
		/// _____________ Elements Gui initialization Area_____________
		window = Window "Title" NilLS
					[	
					  WindowId windState.windowId,
					  WindowClose quit, 					/// using the quit function defined below.
					  WindowViewSize {w = 8*TILE_SIZE, h = 8*TILE_SIZE}, 	/// defining the size of the window.
					  WindowLook False (paintFun windState),   			/// This will take the state and update state away.
					  WindowMouse (const True) Able (temporaryHandler piece windowid)	/// defines a mouse event system and attach handlingMouseEvent function to it.
					  ]
		
		
		
		
		
		///__________ Window painting functions __________________
		/**
			SelectState = Able | Unable  // if window is active or not.
			UpdateState : see it in the library.. it contains the places needs rendering.
			*Picture is passed when the funciton is called by the window attirbute. Just edit it and return a picture at the end.. drawAt and fillAt are functions that work on any drawable object. other funcitons are in the library.
			Note: This function is called whenever a refresh to the page is needed or we can make the window call it explicitly from somewhere else.
		*/
		
		
		/*Static function for rendering the begening scene when window is loaded.*/
		paintFun :: PiecePicture SelectState UpdateState *Picture -> *Picture  //style 2 more suffecient.
		paintFun piece _ {oldFrame, newFrame} pic
		# newFrameSize = rectangleSize newFrame /// The new Window rectangle size
		# oldFrameSize = rectangleSize oldFrame /// The old Window rectangle size
		| newFrameSize.w > 8*TILE_SIZE || newFrameSize.h > 8*TILE_SIZE = pic ///If window resizing is not impacting the main area of the game don't redraw the whole game.
		# rgbColour = {r =130, g=63, b=59} 
		# pic = setPenColour (RGB rgbColour) pic // this is for setting the colour of the brush.
		= fillPieces (fillBoard pic)
		where
			/// containers area
			piecePainters = {paintPiece piece {x =xpos , y= ypos} \\ xpos <- [0..7], ypos <- [0..7] | (ypos < 2) || (ypos > 5)}
			tile = {box_w = TILE_SIZE, box_h = TILE_SIZE} 	/// eventually a box.
			fillingFunctions = {fillAt {x=ycord*TILE_SIZE, y=xcord*TILE_SIZE} tile \\ xcord <- [0..7] , ycord <- [0..7]|(xcord rem 2 == 0 && ycord rem 2 == 0) || (xcord rem 2 <>  0 && ycord rem 2 <> 0)}
			/// applied functions area
			fillPieces = seqArray piecePainters // pic
			fillBoard = seqArray fillingFunctions //pic
			
		
		
		/// ___________ Rendering Pieces functions
		paintPiece :: PiecePicture !Point2  *Picture -> *Picture
		paintPiece pie coord pic  = arrayPaintPieceAux (arrayPointsAndColours) pic 0//To try the tail recursive way, replace with (pointsAndColours 0 0 [])
		where 
			arrayPointsAndColours = { ((coord.x*TILE_SIZE + yPixel, coord.y*TILE_SIZE + xPixel)
				, getPixelValue (xPixel, yPixel) pie )
				\\ yPixel <- [0..TILE_SIZE] ,  xPixel <- [0..TILE_SIZE] | not ( getPixelValue (xPixel, yPixel) pie == {r=255, g=0, b=255}) }
			
			/*pointsAndColours = [ ((coord.x*TILE_SIZE + yPixel, coord.y*TILE_SIZE + xPixel)
				, getPixelValue (xPixel, yPixel) pie )
				\\ yPixel <- [0..TILE_SIZE] ,  xPixel <- [0..TILE_SIZE] | not ( getPixelValue (xPixel, yPixel) pie == {r=255, g=0, b=255}) ]*/
			
			// Currently using arrayPaintPieceAux...
			paintPieceAux :: [((Int ,Int) , RGBColour )] *Picture -> *Picture
			paintPieceAux [] pic = pic
			paintPieceAux [( (x,y) , rgb ) : rest ] pic
			# pic = setPenColour (RGB rgb) pic 
			= paintPieceAux rest (drawPointAt {x = x, y = y} pic)
			
			arrayPaintPieceAux :: {((Int ,Int) , RGBColour )} *Picture Int -> *Picture
			arrayPaintPieceAux arr pic ind
			| ind >= size (arr) = pic
			# ((x,y),rgb) = arr.[ind]
			# pic = setPenColour (RGB rgb) pic 
			= arrayPaintPieceAux arr (drawPointAt {x = x, y = y} pic) (ind+1)
		
		getPixelValue ::  (Int, Int) PiecePicture -> RGBColour
		getPixelValue (x,y) piece
		# xRatio = TILE_SIZE / piece.tileWidth
		# yRatio = TILE_SIZE / piece.tileHeight
		# index =  (x/xRatio) + (y/yRatio) * piece.tileWidth
		| index >= size piece.arrayOfPixels = {r=0, g=0, b=0}
		= piece.arrayOfPixels.[index]
	

 

	/// ________________ Temporary Functions! ____________
	
		/// This temporary Handler should be in place of the mouse handler,
		/// The windowID and the sprite (PiecePicture) should be in the GameState.
		///<
		temporaryHandler :: PiecePicture !Id MouseState (.ls, *PSt .l) -> (.ls,*PSt .l)
		temporaryHandler piece winid (MouseDown hitPoint _ _) pst=:(ls,ps=:{io})
		# pointoo = {x = TILE_SIZE * (hitPoint.x/TILE_SIZE) , y = TILE_SIZE * (hitPoint.y/TILE_SIZE)}
		# io = appWindowPicture winid (paintPiece piece {x=(hitPoint.x / TILE_SIZE),y = (hitPoint.y / TILE_SIZE)}) io
		=  ( ls, {ps & io = setWindowLook winid False (False, paintFun piece) io})
		temporaryHandler _ _ _ pst =  pst
	
	
	
	*/
	
	
	
	
	
	
	
	
	
	
	