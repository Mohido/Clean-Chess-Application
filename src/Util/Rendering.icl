implementation module Util.Rendering

import StdEnv, StdIO, Util.Constants, Util.CostumFunctions

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





/**
For further game Optimisation plans:-
*	fillBoardAt :: !Rectangle !Colour *Picture -> *Picture
*	clearAt :: !Rectangle !Colour *Picture -> *Picture
*	paintPiecesAt :: !Rectangle !Board *Picture -> *Picture
*/


