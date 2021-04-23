implementation module Util.GameOverFunctions

import StdEnv, StdIO, Util.Constants


/*
* A function that takes an index of the player and gamestate.
* It checks if the King of the given index player is under check or not
*/
isUnderCheck :: !Int (*PSt GameState) -> Bool
isUnderCheck p_ind pst=:{ls=gs, io} = length (fst (getCriticalPieces p_ind pst)) <> 0


//Get Pieces that are doing a check on the king of the given index player
getCriticalPieces :: !Int (*PSt GameState) -> ([Piece], (*PSt GameState))
getCriticalPieces p_ind pst=:{ls=gs,io}
# king_p = searchKing gs.worldMatrix gs.players.[p_ind].colour
= backtrace king_p pst
where
	searchKing :: !Board !PieceColour -> Piece
	searchKing board colour = searchKingAux board colour 0
	where
		searchKingAux board colour ind
		| ind >= size board = abort "No King is found!!!"
		| isNothing board.[ind] = searchKingAux board colour (ind+1)
		| (fromJust board.[ind]).player <> colour = searchKingAux board colour (ind+1)
		= (fromJust board.[ind]) /// piece found
	
	backtrace :: Piece (*PSt GameState) -> ([Piece], (*PSt GameState))
	backtrace king_p pst=:{ls=gs, io}
	# creticalPieces = (backtrace_North king_p (king_p.xCord, king_p.yCord) gs.worldMatrix) //up
					/*++ (backtrace_South king_p (king_p.xCord, king_p.yCord) gs.worldMatrix)	//down
					++ (backtrace_East king_p (king_p.xCord, king_p.yCord) gs.worldMatrix)  //right
					++ (backtrace_West king_p (king_p.xCord, king_p.yCord) gs.worldMatrix)	//left
					++ (backtrace_NorthWest king_p (king_p.xCord, king_p.yCord) gs.worldMatrix) //topleft
					++ (backtrace_NorthEast king_p (king_p.xCord, king_p.yCord) gs.worldMatrix) //topright
					++ (backtrace_SouthWest king_p (king_p.xCord, king_p.yCord) gs.worldMatrix)	//bottomleft
					++ (backtrace_SouthEast king_p (king_p.xCord, king_p.yCord) gs.worldMatrix) //bottomright
					++ (backtrace_Knight king_p (king_p.xCord, king_p.yCord) gs.worldMatrix) // knight dirty moves
					*/
	= (creticalPieces, pst)

	/*________________ BackTracing functions for backtracing*/
	
	backtrace_North :: Piece (Int, Int) !Board -> [Piece]
	backtrace_North king_p (xCord, yCord) board
	| xCord > 7 || yCord > 7 || xCord < 0 || yCord < 0 = []
	# index = xCord + yCord * 8
	| isNothing board.[index] = backtrace_North king_p (xCord, yCord - 1) board
	| (fromJust board.[index]).player == king_p.player = []
	=  case (fromJust board.[index]).type of	
			Rook = [fromJust board.[index]]
			Queen = [fromJust board.[index]]
			_ = []
	























