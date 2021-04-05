definition module Pices.main_pieces

import StdEnv, StdIO

::ChessType = Rook | Knight | Bishop | Queen | King | Pawn | NotPiece

instance == ChessType
where
	(==) Rook Rook = True
	(==) Knight Knight = True
	(==) Bishop Bishop = True
	(==) Queen Queen = True
	(==) Pawn Pawn = True
	(==) NotPiece NotPiece = True
	(==) King King = True
	(==) _ _ = False
	
::PieceColour = BlackPiece | WhitePiece | ColourError

instance == PieceColour
where 
	(==) BlackPiece BlackPiece = True
	(==) WhitePiece WhitePiece = True
	(==) _ _ = False
	
::PiecePicture = {
			 tileWidth::Int  , 
			 tileHeight:: Int , 
			 arrayOfPixels :: {RGBColour} 
		   }	

:: Piece = {
	xCord:: Int, 
	yCord :: Int,
	player:: PieceColour, 
	type:: ChessType,
	sprite:: PiecePicture
	}


//Move :: !Piece (!.ls, *PSt .l) -> (!.ls, *PSt .l)


