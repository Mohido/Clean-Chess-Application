implementation module Util.Rendering

import StdEnv, StdIO, Util.Constants, Util.CostumFunctions, Util.Highlights, Util.Dialogs, Util.Castling, Util.Sounds


/*Main Highlighting Function*/

showValidMoves :: Bool (*PSt GameState) ->(*PSt GameState)
showValidMoves realtime pst=:{ls, io} =
					case ls.selectedPiece of
					Nothing = pst
					Just p = case p.type of
								 Rook   = highLightRook realtime pst p 
								 Pawn   = highLightPawn realtime pst p
								 Bishop = HighlightBishop realtime pst p
								 Knight = HighlightKnight realtime pst p
								 Queen  = HighlightQueen realtime pst p
								 King   = HighlightKing realtime pst p
								
								
/*________Rendering Function_____*/

//fillBoardAt :: !Board !Rectangle !Colour *Picture -> *Picture
//fillBoardAt board {corner1, corner2} color pic 
/**
* Painting the window's context. Once it critically needs updating (on creation and resizing)
*/
look :: (!Bool,!Board) SelectState UpdateState *Picture -> *Picture
look (all, board) _ {oldFrame, newFrame, updArea} pic
| all = fillPieces board (fillBoard b_col_1 (clear b_col_2 pic))
# topLeft     = {x = min oldFrame.corner2.x newFrame.corner2.x,
			 		y = min oldFrame.corner1.y newFrame.corner1.y} 				//TopLeft of the right portion
# bottomRight = {x = max oldFrame.corner2.x newFrame.corner2.x,
			 	 	y = max oldFrame.corner2.y newFrame.corner2.y} 			//BottomRight of the right portion
# rightPortion = (lookRectangle {corner1 = topLeft, corner2 = bottomRight} board pic)  // filling right portion of the screen
# topLeft = {x = min oldFrame.corner1.x newFrame.corner1.x,	
			 		y = min oldFrame.corner2.y newFrame.corner2.y}							/// Top left of bottom version
# bottomRight = {x = min oldFrame.corner2.x newFrame.corner2.x,
			 		y = max oldFrame.corner2.y newFrame.corner2.y}							/// BottomRight of the bottom portion
# bottomPortion = (lookRectangle {corner1 = topLeft, corner2 = bottomRight} board rightPortion) 
= bottomPortion // it contains teh right portion as well .......................... here ^ ......
where
	b_col_1 = RGB scndBoardColour															///Board first colour
	b_col_2 = RGB boardColour	
			 	 


/*Start from x and y and fill toward the end of the rectangle.*/	
lookRectangle:: !Rectangle !Board *Picture -> *Picture
lookRectangle rec=:{corner1, corner2} board pic 
= fillPiecesFromTo (xStartCord, yStartCord) (xEndCord, yEndCord) board (tileIt (clearedBoard pic)) /* Clearing => Drawing Tiles => Drawing pieces*/
where
	b_col_1 = RGB scndBoardColour										
	b_col_2 = RGB boardColour
	clearedBoard pic = (clearRect canvas b_col_2 pic)					/// clearing
	tileIt pic = fillRect (xStartCord, yStartCord) (xEndCord, yEndCord) b_col_1 pic /// drawing tiles
	(xStartCord, yStartCord) = ( toInt ((toReal corner1.x / toReal TILE_SIZE) - 0.5) , toInt ((toReal corner1.y / toReal TILE_SIZE) - 0.5)) 	/// to Cooordinates from top left wiith flooring
	(xEndCord, yEndCord) = ( toInt ((toReal corner2.x / toReal TILE_SIZE) + 0.4) , toInt ((toReal corner2.y / toReal TILE_SIZE) + 0.4))			/// to cordinates from the bottom right with ceiling
	canvas = {  corner1 = {x = xStartCord * TILE_SIZE, y = yStartCord* TILE_SIZE} 
			, corner2 = {x = xEndCord * TILE_SIZE, y = yEndCord* TILE_SIZE} 
			} /* Passed to the clearRect function*/


/**
* Clear a part of the screen
*/
clearRect :: Rectangle Colour *Picture -> *Picture
clearRect rec White pic = unfill rec pic
clearRect rec c_col pic
# pic = setPenColour c_col pic
= fill rec pic


/**
* Filling part of the board with tiles. Takes Cordinate system!
*/
fillRect :: (!Int, !Int) (!Int, !Int) Colour *Picture -> *Picture
fillRect (xStartCord, yStartCord) (xEndCord, yEndCord) c_col pic 
# pic = setPenColour c_col pic
= foldr fillTile pic pix_cord_list
where
	pix_cord_list = [ {x=xcord*TILE_SIZE, y=ycord*TILE_SIZE} \\ xcord <- [xStartCord .. min xEndCord 7] , ycord <- [yStartCord .. min 7 yEndCord] 
						| 
					(xcord rem 2 == 0 && ycord rem 2 == 0) || (xcord rem 2 <>  0 && ycord rem 2 <> 0)]
	
	/*Op*/
	fillTile :: !Point2 -> *Picture -> *Picture
	fillTile pix_cord = fillAt pix_cord tile


/**	
* fill the board with pieces starting from the given coordinates till the last given coordinate
*/
fillPiecesFromTo :: (Int, Int) (Int, Int) !Board *Picture -> *Picture
fillPiecesFromTo (xStartCord, yStartCord) (xEndCord, yEndCord) board pic 
| xStartCord > 7 || yStartCord > 7 = pic
= foldr renderPiece pic pieces
where
	pieces = [ board.[xcord + ycord*8] \\ xcord <- [xStartCord .. min (xEndCord-1) 7], ycord <- [yStartCord .. min (yEndCord - 1) 7] ]
 	testing = trace_n (toString (length pieces) ) pieces




/// __________________ LEGACY BOARD RENDERING _____________________
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

/// ___________________________________________ PIECE RENDERING AREA 

/**
* Renders a specific piece.
*/
renderPiece :: !(Maybe Piece) *Picture -> *Picture
renderPiece Nothing pic = pic
renderPiece (Just piece) pic = foldr fillPixel pic pixelsValues
where
	 pixelsValues = [ (y + piece.xCord * TILE_SIZE, x + TILE_SIZE * piece.yCord, getPixelValue (x, y) piece.sprite) 
	 					\\  x <- [0..TILE_SIZE] ,y <- [0..TILE_SIZE] 
	 					|	not (getPixelValue (x, y) piece.sprite == {r=255,g=0,b=255})]
	 					
	fillPixel inp
	# xx = fst3 inp
	# yy = snd3 inp
	# c_rgb = thd3 inp
	= drawPointAt {x=xx, y=yy} o setPenColour (RGB c_rgb)



/*UNNEEDED FUNCTION SINCE THE RENDER FUNCTION UBOVE IT RENDERS ACCORDING TO THE SAVED COORDINATES IN THE PIECE ITSELF*/
///*Takes two Coordinates and draws the piece at the selected coordinates*/
renderPieceAt :: Int Int !Piece *Picture -> *Picture
renderPieceAt xC yC piece pic = foldr fillPixel pic pixelsValues
where
	 pixelsValues = [ (y + xC * TILE_SIZE, x + TILE_SIZE * yC, getPixelValue (x, y) piece.sprite) 
	 					\\  x <- [0..TILE_SIZE] ,y <- [0..TILE_SIZE] 
	 					|	not (getPixelValue (x, y) piece.sprite == {r=255,g=0,b=255})]


/**
* Gets the pixel value related to the TILE_SIZE pixel coordinate system from
* the piece sprite
*/
getPixelValue ::  (!Int, !Int) !PiecePicture -> RGBColour
getPixelValue (x,y) piece
# xRatio = TILE_SIZE / piece.tileWidth
# yRatio = TILE_SIZE / piece.tileHeight
# index =  (x/xRatio) + (y/yRatio) * piece.tileWidth
| index >= size piece.arrayOfPixels = {r=0, g=0, b=0}
= piece.arrayOfPixels.[index]







// ______________________ UPDATING LOGIC (HALF RENDERING HALF LOGIC UPDATING)


/*
*Custom function edits that help with moving pieces
*/

/// Function to Completely Update the world
UpdateGST :: Int Int (*PSt GameState) -> (*PSt GameState)
UpdateGST mouseUpxCord mouseUpyCord pst=:{ls=gs, io}
| mouseUpxCord == selectedxCord && selectedyCord == mouseUpyCord 		= pst
| ((piece.type == Pawn) && ((mouseUpyCord == 7) ||(mouseUpyCord == 0))) = playSoundpromotion proPst
| piece.type == Rook    && mouseUpyCord <> 7 = playSound (disableRightCastle lastPst)
| piece.type == Rook    && mouseUpyCord <> 0 = playSound (disableLeftCastle lastPst) 
| piece.type == King    && (mouseUpxCord <> 6 && mouseUpyCord <> selectedyCord)  && (mouseUpxCord <> 2 && mouseUpyCord <> selectedyCord) = disableBothCastle lastPst
| piece.type == King = checkCastle mouseUpxCord mouseUpyCord lastPst
= playSound lastPst
where
	playSound					  = case gs.worldMatrix.[mouseUpxCord + mouseUpyCord*8] of
									Nothing = playSoundmove
									Just p  = playSoundCapture
	proPst 		 				  = promotion mouseUpxCord mouseUpyCord pst
	piece		 				  = fromJust gs.selectedPiece
	(selectedxCord,selectedyCord) = (piece.xCord,piece.yCord)
	pieceRendered 				  = MovePiece     (selectedxCord,selectedyCord) mouseUpxCord mouseUpyCord Nothing proPst
	updatedIo     				  = setWindowLook (pieceRendered.ls.windowId) False (False, look (False,pieceRendered.ls.worldMatrix)) pieceRendered.io
	lastPst       				  = {pieceRendered & io = updatedIo}


/*Takes oldCoordinates, new Coordinates, and changes the piece's coordinates as well as rendering it in the new place*/
MovePiece :: (Int,Int) Int Int (!Maybe Piece) (*PSt GameState) -> (*PSt GameState)
MovePiece    (selectedxCord,selectedyCord) mouseUpxCord mouseUpyCord piece pst=:{ls, io} 
= lastPst
where
	piece2	    = if (isNothing piece) (fromJust ls.selectedPiece) (fromJust piece)
	updatedPst  = updateWorldMatrix (mouseUpxCord, mouseUpyCord) (selectedxCord + selectedyCord * 8) pst 		//Piece is moved in the world matrix
	erasePiece  = fillFunc mouseUpxCord mouseUpyCord updatedPst 
	pieceMoved  = MovePieceFunc mouseUpxCord mouseUpyCord piece2 erasePiece
	prelastPst  = fillFunc selectedxCord selectedyCord pieceMoved
	lastPst 	= {prelastPst & ls.selectedPiece = Nothing} 



/*____________Aux Functions__________*/
//*Takes two Coordinates and a processState and fills the Board in the coordinates with the appropriate color*/
fillFunc :: Int Int (*PSt GameState) -> (*PSt GameState)
fillFunc xC yC pst=:{ls, io} = {pst & io = appWindowPicture (ls.windowId) (fillBoardAt xC yC) io}


//*Takes two Coordinates and fills the Board Accordingly*/
fillBoardAt :: Int Int *Picture -> *Picture
fillBoardAt xC yC pic 
|(xC rem 2 == 0 && yC rem 2 == 0) || (xC rem 2 <>  0 && yC rem 2 <> 0) =  DrawWhite xC yC pic
=DrawColour xC yC pic
where
	DrawColour xC yC pic
	#ourColour = RGB boardColour
	# pic = setPenColour ourColour pic
	= fillAt {x= xC*TILE_SIZE, y= yC*TILE_SIZE} tile pic
	DrawWhite xC yC pic
	# pic = setPenColour (RGB scndBoardColour) pic
	= fillAt {x= xC*TILE_SIZE, y= yC*TILE_SIZE} tile pic


//*Takes two coordinates and draws the piece over there (An Aux for the previous function)*/	
MovePieceFunc :: Int Int !Piece (*PSt GameState) -> (*PSt GameState)
MovePieceFunc xC yC piece pst=:{ls, io} = {pst & io = appWindowPicture (ls.windowId) (renderPieceAt xC yC piece) io}


	









