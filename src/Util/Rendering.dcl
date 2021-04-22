definition module Util.Rendering

import StdIO, StdEnv, Util.Constants, Util.CostumFunctions


/*___Main Highlighting Function___*/
showValidMoves :: (*PSt GameState) ->(*PSt GameState)


/**
* Painting the window's context. Once it critically needs updating (on creation and resizing)
*/
look :: (!Bool,!Board) SelectState UpdateState *Picture -> *Picture


/**
* clear the whole picture with the given colour
*/
clear :: Colour *Picture -> *Picture


/**
* Fill the whole board as fast as possible.
*/
fillBoard :: Colour *Picture -> *Picture

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

/*
*Custom function edits that help with moving pieces
*/

//*Takes two Coordinates and a processState and fills the Board in the coordinates with the appropriate color*/
fillFunc :: Int Int (*PSt GameState) -> (*PSt GameState)

//*Takes two Coordinates and fills the Board Accordingly*/
fillBoardAt :: Int Int *Picture -> *Picture

//*Takes a piece's old coords and new coords and moves it to the new coords*/
MovePiece :: (Int,Int) Int Int !Piece (*PSt GameState) -> (*PSt GameState)

//*Takes two coordinates and draws the piece over there*/	
MovePieceFunc :: Int Int !Piece (*PSt GameState) -> (*PSt GameState)

///*Takes two Coordinates and draws the piece at the selected coordinates (An Aux for the previous function)*/
renderPieceAt :: Int Int !Piece *Picture -> *Picture

/// Function to Completely Update the world using all of the previous functions
UpdateGST :: Int Int (*PSt GameState) -> (*PSt GameState)

/**
For further game Optimisation plans:-
*	fillBoardAt :: !Rectangle !Colour *Picture -> *Picture
*	clearAt :: !Rectangle !Colour *Picture -> *Picture
*	paintPiecesAt :: !Rectangle !Board *Picture -> *Picture
*/





