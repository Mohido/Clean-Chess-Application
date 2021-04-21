implementation module Util.Rendering

import StdEnv, StdIO, Util.Constants, Util.CostumFunctions, Util.Highlights, Util.Dialogs


/*Main Highlighting Function*/

showValidMoves :: (*PSt GameState) ->(*PSt GameState)
showValidMoves pst=:{ls, io} =
					case ls.selectedPiece of
					Nothing = pst
					Just p = case p.type of
								 Rook   = highLightRook pst p 
								 Bishop = HighlightBishop pst p
								 Pawn   = highLightPawn pst p
								 Knight = HighlightKnight pst p
								 Queen  = HighlightQueen pst p
								 King   = HighlightKing pst p
								
								
/*________Rendering Function_____*/

//fillBoardAt :: !Board !Rectangle !Colour *Picture -> *Picture
//fillBoardAt board {corner1, corner2} color pic 
/**
* Painting the window's context. Once it critically needs updating (on creation and resizing)
*/
look :: !Board SelectState UpdateState *Picture -> *Picture
look board _ {oldFrame, newFrame, updArea} pic
# newFrameSize	= rectangleSize newFrame 				/// The new Window rectangle size
# oldFrameSize 	= rectangleSize oldFrame 				/// The old Window rectangle size
| newFrameSize.w > 8*TILE_SIZE || 	
		newFrameSize.h > 8*TILE_SIZE  = pic 			///If window resizing is not impacting the main area of the game don't redraw the whole game.
# b_col_1 = White										///Board first colour
# b_col_2 = RGB {r =130, g=63, b=59} 					///Board sec.. colour
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

/*
*Custom function edits that help with moving pieces
*/


/// Function to Completely Update the world
UpdateGST :: Int Int (*PSt GameState) -> (*PSt GameState)
UpdateGST mouseUpxCord mouseUpyCord pst=:{ls=gs, io}
| mouseUpxCord == selectedxCord && selectedyCord == mouseUpyCord 		= pst
| ((piece.type == Pawn) && ((mouseUpyCord == 7) ||(mouseUpyCord == 0))) = proPst
= lastPst
where
	piece		 				  = fromJust gs.selectedPiece
	proPst 		 				  = promotion mouseUpxCord mouseUpyCord pst
	(selectedxCord,selectedyCord) = (piece.xCord,piece.yCord)
	pieceRendered 				  = MovePiece     (selectedxCord,selectedyCord) mouseUpxCord mouseUpyCord piece proPst
	updatedIo     				  = setWindowLook (pieceRendered.ls.windowId) False (False, look pieceRendered.ls.worldMatrix) pieceRendered.io
	lastPst       				  = {pieceRendered & io = updatedIo}


/*Takes oldCoordinates, new Coordinates, and changes the piece's coordinates as well as rendering it in the new place*/
MovePiece :: (Int,Int) Int Int !Piece (*PSt GameState) -> (*PSt GameState)
MovePiece    (selectedxCord,selectedyCord) mouseUpxCord mouseUpyCord piece pst=:{ls, io}
 = lastPst
where
	updatedPst  = updateWorldMatrix (mouseUpxCord, mouseUpyCord) (selectedxCord + selectedyCord * 8) pst 		//Piece is moved in the world matrix
	erasePiece  = fillFunc mouseUpxCord mouseUpyCord updatedPst 
	pieceMoved  = MovePieceFunc mouseUpxCord mouseUpyCord piece erasePiece
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
	#ourColour = RGB{r =130, g=63, b=59}
	# pic = setPenColour ourColour pic
	= fillAt {x= xC*TILE_SIZE, y= yC*TILE_SIZE} tile pic
	DrawWhite xC yC pic
	# pic = setPenColour White pic
	= fillAt {x= xC*TILE_SIZE, y= yC*TILE_SIZE} tile pic


//*Takes two coordinates and draws the piece over there (An Aux for the previous function)*/	
MovePieceFunc :: Int Int !Piece (*PSt GameState) -> (*PSt GameState)
MovePieceFunc xC yC piece pst=:{ls, io} = {pst & io = appWindowPicture (ls.windowId) (renderPieceAt xC yC piece) io}

///*Takes two Coordinates and draws the piece at the selected coordinates*/
renderPieceAt :: Int Int !Piece *Picture -> *Picture
renderPieceAt xC yC piece pic = foldr fillPixel pic pixelsValues
where
	 pixelsValues = [ (y + xC * TILE_SIZE, x + TILE_SIZE * yC, getPixelValue (x, y) piece.sprite) 
	 					\\  x <- [0..TILE_SIZE] ,y <- [0..TILE_SIZE] 
	 					|	not (getPixelValue (x, y) piece.sprite == {r=255,g=0,b=255})]

	
/**
For further game Optimisation plans:-
*	fillBoardAt :: !Rectangle !Colour *Picture -> *Picture
*	clearAt :: !Rectangle !Colour *Picture -> *Picture
*	paintPiecesAt :: !Rectangle !Board *Picture -> *Picture
*/


