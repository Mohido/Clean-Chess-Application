implementation module Pices.main_pieces

import StdEnv, StdIO 


/*
Move :: Piece (.ls, *PSt .l) -> (.ls, *PSt .l)
Move chess_piece (local, pst)
	//| chess_piece.chessPiece == Rook = MoveRook chess_piece (local, pst)
	//| chess_piece.chessPiece == Knight = MoveKnight chess_piece (local, pst)
	//| chess_piece.chessPiece == Bishop = MoveBishop chess_piece (local, pst)
	//| chess_piece.chessPiece == Queen = MoveQueen chess_piece (local, pst)
	//| chess_piece.chessPiece == Pawn = MovePawn chess_piece (local, pst)
	= (local, pst)
	
	*/