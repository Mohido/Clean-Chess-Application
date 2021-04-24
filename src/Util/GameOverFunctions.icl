implementation module Util.GameOverFunctions

import StdEnv, StdIO, Util.Constants, StdDebug, Util.Rendering






/*
* Takes the original PST with no selected piece or whatsoever, then bruteforce to see if there is a CheckMate
* PieceColour : The player u want to check if he is under a checkmate
*/
isGameOver :: PieceColour (*PSt GameState) -> (Bool , (*PSt GameState) )
isGameOver p_colour pst=:{ls}
# pieces_list = [ p \\ p <-: ls.worldMatrix | not (isNothing p) && (fromJust p).player == p_colour ]
= searchValidMoves pieces_list pst
where
	searchValidMoves:: [(Maybe Piece)] (*PSt GameState) -> (Bool, (*PSt GameState) )
	searchValidMoves [] pst = trace_n "\n" (True, pst)
	searchValidMoves [p:rest] pst=:{ls=gs}
	# tempPst = {pst & ls = {gs & selectedPiece = p}}
	# validPst = showValidMoves False tempPst
	# (game_s, validPst) = getGameState validPst
	# foundValid = or [ move \\ move <-: game_s.validMoves]
	# orig_pst = {validPst & ls = gs}
	| foundValid = trace_n ( "Checking: " +++ toString (fromJust p) +++ "\n") (False, orig_pst) /// game not over
	=trace_n ( "Checking: " +++ toString (fromJust p)) (searchValidMoves rest orig_pst)



/*
* A function that takes an index of the player and gamestate.
* It checks if the King of the given index player is under check or not
*/
isUnderCheck :: PieceColour Board -> Bool
isUnderCheck colour board
# pieces_list = getCriticalPieces colour board
# isCheck = length pieces_list <> 0
= isCheck


//Get Pieces that are doing a check on the king of the given index player
getCriticalPieces :: PieceColour Board -> [Piece]
getCriticalPieces colour board
# king_p = searchKing board colour
# pieces_list = backtrace king_p board
= pieces_list 
where
	///for testing and debuging 
	printPieces :: [Piece] -> String
	printPieces [] = ""
	printPieces [p:rest] = toString p +++ printPieces rest
	
	
	searchKing :: !Board !PieceColour -> Piece
	searchKing board colour = searchKingAux board colour 0
	where
		searchKingAux board colour ind
		| ind >= size board = abort "No King is found!!!"
		| isNothing board.[ind] = searchKingAux board colour (ind+1)
		| (fromJust board.[ind]).player <> colour || (fromJust board.[ind]).type <> King = searchKingAux board colour (ind+1)
		= (fromJust board.[ind]) /// piece found
	
	backtrace :: Piece Board -> [Piece]
	backtrace king_p board
	# creticalPieces = (backtrace_North king_p (king_p.xCord, king_p.yCord - 1) board) //up
					++ (backtrace_South king_p (king_p.xCord, king_p.yCord + 1) board)	//down
					++ (backtrace_East king_p (king_p.xCord + 1, king_p.yCord) board)  //right
					++ (backtrace_West king_p (king_p.xCord - 1, king_p.yCord) board)	//left
					++ (backtrace_NorthWest king_p (king_p.xCord - 1, king_p.yCord - 1) board) //topleft
					++ (backtrace_NorthEast king_p (king_p.xCord + 1, king_p.yCord - 1) board) //topright
					++ (backtrace_SouthWest king_p (king_p.xCord - 1, king_p.yCord + 1) board)	//bottomleft
					++ (backtrace_SouthEast king_p (king_p.xCord + 1, king_p.yCord + 1) board) //bottomright
					++ (check_Knight king_p (king_p.xCord + 2, king_p.yCord - 1) board) // knight x + 2, y - 1   	-*
					++ (check_Knight king_p (king_p.xCord + 2, king_p.yCord + 1) board) // Knight x + 2 , y + 1  	-.
					++ (check_Knight king_p (king_p.xCord + 1, king_p.yCord + 2) board) // knight y + 2, x + 1 	|.
					++ (check_Knight king_p (king_p.xCord - 1, king_p.yCord + 2) board) // knight y + 2, x - 1 	.|
					++ (check_Knight king_p (king_p.xCord + 1, king_p.yCord - 2) board) // knight y - 2, x + 1 	|*
					++ (check_Knight king_p (king_p.xCord - 1, king_p.yCord - 2) board) // knight y - 2, x - 1 	*|
					++ (check_Knight king_p (king_p.xCord - 2, king_p.yCord - 1) board) // knight x - 2, y - 1 	*-
					++ (check_Knight king_p (king_p.xCord - 2, king_p.yCord + 1) board) // knight x - 2, y - 1 	.-
	= creticalPieces

	/*________________ BackTracing functions for backtracing*/
	
	backtrace_North :: Piece (Int, Int) !Board -> [Piece]
	backtrace_North king_p (xCord, yCord) board
	| xCord > 7 || yCord > 7 || xCord < 0 || yCord < 0 = []
	# index = xCord + yCord * 8
	| isNothing board.[index] = backtrace_North king_p (xCord, yCord - 1) board
	| (fromJust board.[index]).player == king_p.player = []
	# piece = (fromJust board.[index])
	=  case (fromJust board.[index]).type of	
			Rook = [fromJust board.[index]]
			Queen = [fromJust board.[index]]
			King = if ( king_p.xCord == xCord && king_p.yCord - 1 == yCord) [piece] []
			_ = []
	

	backtrace_South :: Piece (Int, Int) !Board -> [Piece]
	backtrace_South king_p (xCord, yCord) board
	| xCord > 7 || yCord > 7 || xCord < 0 || yCord < 0 = []
	# index = xCord + yCord * 8
	| isNothing board.[index] = backtrace_South king_p (xCord, yCord + 1) board
	| (fromJust board.[index]).player == king_p.player = []
	# piece = (fromJust board.[index])
	=  case (fromJust board.[index]).type of	
			Rook = [fromJust board.[index]]
			Queen = [fromJust board.[index]]
			King = if ( king_p.xCord == xCord && king_p.yCord + 1 == yCord) [piece] []
			_ = []


	backtrace_East :: Piece (Int, Int) !Board -> [Piece]
	backtrace_East king_p (xCord, yCord) board
	| xCord > 7 || yCord > 7 || xCord < 0 || yCord < 0 = []
	# index = xCord + yCord * 8
	| isNothing board.[index] = backtrace_East king_p (xCord + 1, yCord) board
	| (fromJust board.[index]).player == king_p.player = []
	# piece = (fromJust board.[index])
	=  case (fromJust board.[index]).type of	
			Rook = [fromJust board.[index]]
			Queen = [fromJust board.[index]]
			King = if ( king_p.xCord + 1 == xCord && king_p.yCord == yCord) [piece] []
			_ = []

	backtrace_West :: Piece (Int, Int) !Board -> [Piece]
	backtrace_West king_p (xCord, yCord) board
	| xCord > 7 || yCord > 7 || xCord < 0 || yCord < 0 = []
	# index = xCord + yCord * 8
	| isNothing board.[index] = backtrace_West king_p (xCord - 1, yCord) board
	| (fromJust board.[index]).player == king_p.player = []
	# piece = (fromJust board.[index])
	=  case (fromJust board.[index]).type of	
			Rook = [fromJust board.[index]]
			Queen = [fromJust board.[index]]
			King = if ( king_p.xCord - 1 == xCord && king_p.yCord == yCord) [piece] []
			_ = []

	/*Diagonals*/
	backtrace_NorthWest :: Piece (Int, Int) !Board -> [Piece]
	backtrace_NorthWest king_p (xCord, yCord) board
	| xCord > 7 || yCord > 7 || xCord < 0 || yCord < 0 = []
	# index = xCord + yCord * 8
	| isNothing board.[index] = backtrace_NorthWest king_p (xCord - 1, yCord - 1) board
	| (fromJust board.[index]).player == king_p.player = []
	# piece = (fromJust board.[index])
	=  case piece.type of
			Bishop = [fromJust board.[index]]
			Queen = [fromJust board.[index]]
			King = if ( king_p.xCord - 1 == xCord && king_p.yCord - 1 == yCord) [piece] []
			Pawn = 	case king_p.player of
						BlackPiece = if ( (king_p.xCord + 1 == xCord || king_p.xCord - 1 == xCord ) && king_p.yCord + 1 == yCord) [piece] []
						WhitePiece = if ( (king_p.xCord + 1 == xCord || king_p.xCord - 1 == xCord ) && king_p.yCord - 1 == yCord) [piece] []
			_ = []
	
	
	backtrace_NorthEast :: Piece (Int, Int) !Board -> [Piece]
	backtrace_NorthEast king_p (xCord, yCord) board
	| xCord > 7 || yCord > 7 || xCord < 0 || yCord < 0 = []
	# index = xCord + yCord * 8
	| isNothing board.[index] = backtrace_NorthEast king_p (xCord + 1, yCord - 1) board
	| (fromJust board.[index]).player == king_p.player = []
	# piece = (fromJust board.[index])
	=  case piece.type of
			Bishop = [fromJust board.[index]]
			Queen = [fromJust board.[index]]
			King = if ( king_p.xCord + 1 == xCord && king_p.yCord - 1 == yCord) [piece] []
			Pawn = 	case king_p.player of
						BlackPiece = if ( (king_p.xCord + 1 == xCord || king_p.xCord - 1 == xCord ) && king_p.yCord + 1 == yCord) [piece] []
						WhitePiece = if ( (king_p.xCord + 1 == xCord || king_p.xCord - 1 == xCord ) && king_p.yCord - 1 == yCord) [piece] []
			_ = []


	backtrace_SouthWest :: Piece (Int, Int) !Board -> [Piece]
	backtrace_SouthWest king_p (xCord, yCord) board
	| xCord > 7 || yCord > 7 || xCord < 0 || yCord < 0 = []
	# index = xCord + yCord * 8
	| isNothing board.[index] = backtrace_SouthWest king_p (xCord - 1, yCord + 1) board
	| (fromJust board.[index]).player == king_p.player = []
	# piece = (fromJust board.[index])
	= case piece.type of
			Bishop = [fromJust board.[index]]
			Queen = [fromJust board.[index]]
			King = if ( king_p.xCord - 1 == xCord && king_p.yCord + 1 == yCord) [piece] []
			Pawn = 	case king_p.player of
						BlackPiece = if ( (king_p.xCord + 1 == xCord || king_p.xCord - 1 == xCord ) && king_p.yCord + 1 == yCord) [piece] []
						WhitePiece = if ( (king_p.xCord + 1 == xCord || king_p.xCord - 1 == xCord ) && king_p.yCord - 1 == yCord) [piece] []
			_ = []


	backtrace_SouthEast :: Piece (Int, Int) !Board -> [Piece]
	backtrace_SouthEast king_p (xCord, yCord) board
	| xCord > 7 || yCord > 7 || xCord < 0 || yCord < 0 = []
	# index = xCord + yCord * 8
	| isNothing board.[index] = backtrace_SouthEast king_p (xCord + 1, yCord + 1) board
	| (fromJust board.[index]).player == king_p.player = []
	# piece = (fromJust board.[index])
	=  case piece.type of
			Bishop = [fromJust board.[index]]
			Queen = [fromJust board.[index]]
			King = if ( king_p.xCord + 1 == xCord && king_p.yCord + 1 == yCord) [piece] []
			Pawn = 	case king_p.player of
						BlackPiece = if ( (king_p.xCord + 1 == xCord || king_p.xCord - 1 == xCord ) && king_p.yCord + 1 == yCord) [piece] []
						WhitePiece = if ( (king_p.xCord + 1 == xCord || king_p.xCord - 1 == xCord ) && king_p.yCord - 1 == yCord) [piece] []
			_ = []


	/*Weird chess pieces movements*/
	check_Knight :: Piece (Int, Int) !Board -> [Piece]
	check_Knight king_p (xCord, yCord) board
	| xCord > 7 || yCord > 7 || xCord < 0 || yCord < 0 = []
	# index = xCord + yCord * 8
	| isNothing board.[index] = []
	| (fromJust board.[index]).player == king_p.player = []
	# piece = (fromJust board.[index])
	= if (piece.type == Knight) [piece] []
	
	











