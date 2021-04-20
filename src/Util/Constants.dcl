definition module Util.Constants

import StdEnv, StdIO

//TILE SIZE
TILE_SIZE :== 64


//Window Content Size (Rectangle)
pictDomain :== {corner1 = {x=0		    , y=0},
				corner2 = {x=8*TILE_SIZE, y=8*TILE_SIZE}}  	



tile :== {box_w = TILE_SIZE, box_h = TILE_SIZE}

 

initPlayers :: {Player}

::ChessType = Rook | Knight | Bishop | Queen | King | Pawn | NotPiece

instance == ChessType

initMoves :: {#Bool}
updateBool:: Int {#Bool} -> {#Bool}

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

:: Player = { colour 		 :: PieceColour
			, castleLeft 	 :: Bool
			, castleRight	 :: Bool
			}

:: Board :== {!(Maybe Piece)}


::GameState = {
		turnCount		 :: Int,
		players			 :: {Player},
		worldMatrix      :: !Board ,
		selectedPiece	 :: Maybe Piece,
		windowId 		 :: !Id,
		validMoves 		 :: {#Bool}
	}