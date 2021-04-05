definition module Util.Constants

import StdEnv, StdIO

//TILE SIZE
TILE_SIZE :== 64

::ChessType = Rook | Knight | Bishop | Queen | King | Pawn | NotPiece

instance == ChessType

	
::PieceColour = BlackPiece | WhitePiece | ColourError

instance == PieceColour
instance == RGBColour
	
::PiecePicture = {
			 tileWidth::Int  , 
			 tileHeight:: Int , 
			 arrayOfPixels :: {#RGBColour} 
		   }	

:: Piece = {
	xCord:: Int, 
	yCord :: Int,
	player:: PieceColour, 
	type:: ChessType,
	sprite:: PiecePicture
	}

::GameState = {
			worldMatrix :: {#Piece},
			selectedPiece :: Piece,
			windowId :: !Id
		}
