implementation module Util.Constants

import StdEnv, StdIO

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
	
	
instance == PieceColour
where 
	(==) BlackPiece BlackPiece = True
	(==) WhitePiece WhitePiece = True
	(==) _ _ = False
	
instance == RGBColour
where
	(==) x y = x.r == y.r && x.b == y.b && x.g == y.g
	(==) _ _ = False