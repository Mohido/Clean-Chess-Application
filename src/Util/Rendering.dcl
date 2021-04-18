definition module Util.Rendering

import StdIO, StdEnv, Util.Constants, Util.CostumFunctions


/*___Main Highlighting Function___*/
showValidMoves :: (*PSt GameState) ->(*PSt GameState)


/**
* Painting the window's context. Once it critically needs updating (on creation and resizing)
*/
look :: !Board SelectState UpdateState *Picture -> *Picture


/**
* clear the whole picture with the given colour
*/
clear :: Colour *Picture -> *Picture


/**
* Fill the whole board as fast as possible.
*/
fillBoard :: Colour *Picture -> *Picture


fillBoardAt :: Int Int *Picture -> *Picture
/**
* fill the board with pieces.
*/
fillPieces :: !Board *Picture -> *Picture


/**
* Renders a specific piece.
*/
renderPiece :: !(Maybe Piece) *Picture -> *Picture


/**
* Gets the pixel value related to the TILE_SIZE pixel coordinate system from
* the piece sprite
*/
getPixelValue ::  (!Int, !Int) !PiecePicture -> RGBColour


renderPieceAt :: Int Int !(Maybe Piece) *Picture -> *Picture



/**
For further game Optimisation plans:-
*	fillBoardAt :: !Rectangle !Colour *Picture -> *Picture
*	clearAt :: !Rectangle !Colour *Picture -> *Picture
*	paintPiecesAt :: !Rectangle !Board *Picture -> *Picture
*/








