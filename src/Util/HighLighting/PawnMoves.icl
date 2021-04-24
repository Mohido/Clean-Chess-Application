implementation module Util.HighLighting.PawnMoves 

import StdEnv, StdIO, Util.Constants, Util.GameOverFunctions , Util.CostumFunctions
from StdFunc import seq

								
/*A bit ugly and needs more polishing but this works well for now.*/
highLightPawn :: Bool (*PSt GameState) !Piece -> (*PSt GameState)
highLightPawn hightLight pst=:{ls, io} p 
|p.player == BlackPiece =  seq [ 	  moveForwardPawn hightLight xC (yC+1) p
									, moveDiagonallyPawn hightLight (xC+1) (yC+1) p
									, moveDiagonallyPawn hightLight (xC-1) (yC+1) p
							   ] newPst
= seq [	moveForwardPawn hightLight xC (yC-1) p,
		moveDiagonallyPawn hightLight (xC+1) (yC-1) p,
		moveDiagonallyPawn hightLight (xC-1) (yC-1) p] newPst
where
	xC 	   = (p.xCord) 
	yC 	   = (p.yCord) 
	point  = {x = xC * TILE_SIZE , y = yC * TILE_SIZE}
	newPst     = case hightLight of
					True = {pst & io = appWindowPicture (ls.windowId) (hiliteAt point tile) io} 
					False = pst

	
/*One big function to manage moving forward for both white and black pawn pieces.*/
moveForwardPawn :: Bool Int Int !Piece (*PSt GameState)-> (*PSt GameState)
moveForwardPawn hightLight xC yC p pst=:{ls, io}
|yC < 0 || yC > 7 = pst
|p.player == BlackPiece = moveForwardPawnAux_b hightLight xC yC p pst
	 					= moveForwardPawnAux_w hightLight xC yC p pst

moveForwardPawnAux_b :: Bool Int Int !Piece (*PSt GameState)-> (*PSt GameState)
moveForwardPawnAux_b highLight xC yC p pst=:{ls = gs , io}
# updatedPst = updateWorldMatrix (xC,yC) (p.xCord + p.yCord * 8) pst   // update the world matrix
# (game_s , updatedPst2) = getGameState updatedPst					   // get the new gamestate of the world
# isChecked = isUnderCheck p.player game_s.worldMatrix  			   // check if there is a check in the new world matrix
# orig_Pst = {updatedPst2 & ls = gs}								   // returning back the new gamestate to the old one 
| isChecked = orig_Pst
|(yC == 2) = case highLight of
					True  = case gs.worldMatrix.[xC + yC * 8] of //If pawn is at init pos, can move twice
								Nothing = moveForwardPawnAux_b highLight xC (yC+1) p {orig_Pst & io = appWindowPicture (gs.windowId) (hiliteAt point tile) io
																					 , ls = {gs & validMoves = updateBool (xC + yC * 8) gs.validMoves}} 
								Just piece = orig_Pst
					False = case gs.worldMatrix.[xC + yC * 8] of //If pawn is at init pos, can move twice
								Nothing = moveForwardPawnAux_b highLight xC (yC+1) p {pst & ls = {gs & validMoves = updateBool (xC + yC * 8) gs.validMoves}} 
								Just piece = orig_Pst			

= case highLight of 
		True  = case gs.worldMatrix.[xC + yC * 8] of //Otherwise, can only move once
					Nothing = {orig_Pst & io = appWindowPicture (gs.windowId) (hiliteAt point tile) io, 
										  ls = {gs & validMoves = updateBool (xC + yC * 8) gs.validMoves}}
					Just piece = orig_Pst
		False = case gs.worldMatrix.[xC + yC * 8] of //Otherwise, can only move once
					Nothing = {orig_Pst & ls = {gs & validMoves = updateBool (xC + yC * 8) gs.validMoves}}
					Just piece = orig_Pst
where
	point  = {x = xC * TILE_SIZE , y = yC * TILE_SIZE}

moveForwardPawnAux_w :: Bool Int Int !Piece (*PSt GameState)-> (*PSt GameState)
moveForwardPawnAux_w highLight xC yC p pst=:{ls = gs, io}
# updatedPst = updateWorldMatrix (xC,yC) (p.xCord + p.yCord * 8) pst   // update the world matrix
# (game_s , updatedPst2) = getGameState updatedPst					   // get the new gamestate of the world
# isChecked = isUnderCheck p.player game_s.worldMatrix  			   // check if there is a check in the new world matrix
# orig_Pst = {updatedPst2 & ls = gs}								   // returning back the new gamestate to the old one 
| isChecked = orig_Pst
|(yC == 5) = case highLight of
					True  = case gs.worldMatrix.[xC + yC * 8] of //If pawn is at init pos, can move twice
								Nothing = moveForwardPawnAux_b highLight xC (yC - 1) p {orig_Pst & io = appWindowPicture (gs.windowId) (hiliteAt point tile) io
																					 , ls = {gs & validMoves = updateBool (xC + yC * 8) gs.validMoves}} 
								Just piece = orig_Pst
					False = case gs.worldMatrix.[xC + yC * 8] of //If pawn is at init pos, can move twice
								Nothing = moveForwardPawnAux_b highLight xC (yC - 1) p {pst & ls = {gs & validMoves = updateBool (xC + yC * 8) gs.validMoves}} 
								Just piece = orig_Pst			

= case highLight of 
		True  = case gs.worldMatrix.[xC + yC * 8] of //Otherwise, can only move once
					Nothing = {orig_Pst & io = appWindowPicture (gs.windowId) (hiliteAt point tile) io, 
										  ls = {gs & validMoves = updateBool (xC + yC * 8) gs.validMoves}}
					Just piece = orig_Pst
		False = case gs.worldMatrix.[xC + yC * 8] of //Otherwise, can only move once
					Nothing = {orig_Pst & ls = {gs & validMoves = updateBool (xC + yC * 8) gs.validMoves}}
					Just piece = orig_Pst
where
	point  = {x = xC * TILE_SIZE , y = yC * TILE_SIZE}

/*Moving diagonally with a pawn is quite straight forward.*/
moveDiagonallyPawn :: Bool Int Int !Piece (*PSt GameState)-> (*PSt GameState)
moveDiagonallyPawn highLight xC yC p pst=:{ls = gs, io}
|xC <0 ||xC > 7  || yC < 0 || yC > 7 = pst
# updatedPst = updateWorldMatrix (xC,yC) (p.xCord + p.yCord * 8) pst   // update the world matrix
# (game_s , updatedPst2) = getGameState updatedPst					   // get the new gamestate of the world
# isChecked = isUnderCheck p.player game_s.worldMatrix  			   // check if there is a check in the new world matrix
# orig_Pst = {updatedPst2 & ls = gs}								   // returning back the new gamestate to the old one 
| isChecked = orig_Pst
= case highLight of
			True = case gs.worldMatrix.[xC + yC * 8] of
							Nothing = orig_Pst
							Just piece = case (p.player == piece.player) of
												True = orig_Pst
												False= {orig_Pst & io = appWindowPicture (gs.windowId) (hiliteAt point tile) io, 
																ls = {gs & validMoves = updateBool (xC + yC * 8) gs.validMoves}}
			False = case gs.worldMatrix.[xC + yC * 8] of
							Nothing = orig_Pst
							Just piece = case (p.player == piece.player) of
												True = orig_Pst
												False= {orig_Pst & ls = {gs & validMoves = updateBool (xC + yC * 8) gs.validMoves}}
where
	point  = {x = xC * TILE_SIZE , y = yC * TILE_SIZE}
