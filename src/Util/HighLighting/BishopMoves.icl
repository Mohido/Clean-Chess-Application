implementation module Util.HighLighting.BishopMoves
import StdEnv, StdIO, Util.Constants, Util.CostumFunctions, Util.GameOverFunctions
from StdFunc import seq

//**************************************************************************
// NOTE: functions ForwardMovesAlongMainDiagonal, BackWardMovesAlongMainDiagonal,
// ForwardMovesAlongAntiDiagonal, and BackWardMovesAlongAntiDiagonal must include the following conditions include in the first gaurd: 
//
// isCheckMate
// It should also deal with whether the king is under check: which can only be done when done with the game logic implementation
//******************************************************************************


HighlightBishop :: Bool (*PSt GameState) !Piece -> (*PSt GameState)
HighlightBishop highlight pst=:{ls, io} p = seq [ForwardMovesAlongMainDiagonal highlight (xC-1) (yC+1) p,
										BackWardMovesAlongMainDiagonal highlight (xC+1) (yC-1) p, 
										ForwardMovesAlongAntiDiagonal highlight (xC+1) (yC+1) p, 
										BackWardMovesAlongAntiDiagonal highlight (xC-1) (yC-1) p] newPst
where
	xC 	   = (p.xCord) 
	yC 	   = (p.yCord) 
	point = {x = xC * TILE_SIZE , y = yC * TILE_SIZE}
	newPst     = {pst & io = appWindowPicture (ls.windowId) (hiliteAt point tile) io}



//one-forward-one-to-the-left
ForwardMovesAlongMainDiagonal :: Bool Int Int !Piece (*PSt GameState) -> (*PSt GameState)
ForwardMovesAlongMainDiagonal highlight xC yC p pst=:{ls=gs, io} 
| xC < 0 || yC > 7 = pst
| not (isNothing gs.worldMatrix.[xC + yC * 8]) && (fromJust gs.worldMatrix.[xC + yC * 8]).player == p.player = pst 		// initial check is necessary!
# updatedPst = updateWorldMatrix (xC,yC) (p.xCord + p.yCord * 8) pst   // update the world matrix
# (game_s , updatedPst2) = getGameState updatedPst					   // get the new gamestate of the world
# isChecked = isUnderCheck p.player game_s.worldMatrix  			   // check if there is a check in the new world matrix
# orig_Pst = {updatedPst2 & ls = gs}								   // returning back the new gamestate to the old one 
| isChecked = orig_Pst												   // if there is a check in the new world matrix, then it is an invalid move.
| isNothing gs.worldMatrix.[xC + yC * 8] = case highlight of
												True = ForwardMovesAlongMainDiagonal highlight (xC-1) (yC+1) p {orig_Pst & io = appWindowPicture (gs.windowId) ((hiliteAt point tile)) io , ls = {gs & validMoves = updateBool (xC + yC * 8) gs.validMoves}}  
												False = ForwardMovesAlongMainDiagonal highlight (xC-1) (yC+1) p {orig_Pst & ls = {gs & validMoves = updateBool (xC + yC * 8) gs.validMoves}}  
= case highlight of
	True = {orig_Pst & io = appWindowPicture (gs.windowId) ((hiliteAt point tile)) io , ls = {gs & validMoves = updateBool (xC + yC * 8) gs.validMoves}} 
	False = {orig_Pst & ls = {gs & validMoves = updateBool (xC + yC * 8) gs.validMoves}} 
where
	point = {x = xC * TILE_SIZE , y = yC * TILE_SIZE}
					

//one-back-ward-one-to-the-right
BackWardMovesAlongMainDiagonal :: Bool Int Int !Piece (*PSt GameState) -> (*PSt GameState)
BackWardMovesAlongMainDiagonal highlight xC yC p pst=:{ls=gs, io}
| xC > 7 || yC < 0 = pst
| not (isNothing gs.worldMatrix.[xC + yC * 8]) && (fromJust gs.worldMatrix.[xC + yC * 8]).player == p.player = pst 		// initial check is necessary!
# updatedPst = updateWorldMatrix (xC,yC) (p.xCord + p.yCord * 8) pst   // update the world matrix
# (game_s , updatedPst2) = getGameState updatedPst					   // get the new gamestate of the world
# isChecked = isUnderCheck p.player game_s.worldMatrix  			   // check if there is a check in the new world matrix
# orig_Pst = {updatedPst2 & ls = gs}								   // returning back the new gamestate to the old one 
| isChecked = orig_Pst												   // if there is a check in the new world matrix, then it is an invalid move.
| isNothing gs.worldMatrix.[xC + yC * 8] = case highlight of
												True = BackWardMovesAlongMainDiagonal highlight (xC+1) (yC-1) p {orig_Pst & io = appWindowPicture (gs.windowId) ((hiliteAt point tile)) io , ls = {gs & validMoves = updateBool (xC + yC * 8) gs.validMoves}}  
												False = BackWardMovesAlongMainDiagonal highlight (xC+1) (yC-1) p {orig_Pst & ls = {gs & validMoves = updateBool (xC + yC * 8) gs.validMoves}}  
= case highlight of
	True = {orig_Pst & io = appWindowPicture (gs.windowId) ((hiliteAt point tile)) io , ls = {gs & validMoves = updateBool (xC + yC * 8) gs.validMoves}} // if not the same piece Highlight and stop
	False = {orig_Pst & ls = {gs & validMoves = updateBool (xC + yC * 8) gs.validMoves}} // if not the same piece Highlight and stop
where
	point = {x = xC * TILE_SIZE , y = yC * TILE_SIZE}




//one-forward-one-to-the-right
ForwardMovesAlongAntiDiagonal:: Bool Int Int !Piece (*PSt GameState)-> (*PSt GameState)
ForwardMovesAlongAntiDiagonal highlight xC yC p pst=:{ls=gs, io}
| yC > 7 || xC > 7 = pst
| not (isNothing gs.worldMatrix.[xC + yC * 8]) && (fromJust gs.worldMatrix.[xC + yC * 8]).player == p.player = pst 		// initial check is necessary!
# updatedPst = updateWorldMatrix (xC,yC) (p.xCord + p.yCord * 8) pst   // update the world matrix
# (game_s , updatedPst2) = getGameState updatedPst					   // get the new gamestate of the world
# isChecked = isUnderCheck p.player game_s.worldMatrix  			   // check if there is a check in the new world matrix
# orig_Pst = {updatedPst2 & ls = gs}								   // returning back the new gamestate to the old one 
| isChecked = orig_Pst												   // if there is a check in the new world matrix, then it is an invalid move.
| isNothing gs.worldMatrix.[xC + yC * 8] = case highlight of
												True = ForwardMovesAlongAntiDiagonal highlight (xC + 1) (yC+1) p {orig_Pst & io = appWindowPicture (gs.windowId) ((hiliteAt point tile)) io , ls = {gs & validMoves = updateBool (xC + yC * 8) gs.validMoves}}  
												False = ForwardMovesAlongAntiDiagonal highlight (xC + 1) (yC+1) p {orig_Pst & ls = {gs & validMoves = updateBool (xC + yC * 8) gs.validMoves}}  
= case highlight of 
	True = {orig_Pst & io = appWindowPicture (gs.windowId) ((hiliteAt point tile)) io , ls = {gs & validMoves = updateBool (xC + yC * 8) gs.validMoves}} // if not the same piece Highlight and stop
	False = {orig_Pst & ls = {gs & validMoves = updateBool (xC + yC * 8) gs.validMoves}} // if not the same piece Highlight and stop
where
	point = {x = xC * TILE_SIZE , y = yC * TILE_SIZE}



//one-backward-one-to-the-left
BackWardMovesAlongAntiDiagonal :: Bool Int Int !Piece (*PSt GameState)-> (*PSt GameState)
BackWardMovesAlongAntiDiagonal highlight xC yC p pst=:{ls=gs, io} 
| yC < 0 || xC < 0 = pst
| not (isNothing gs.worldMatrix.[xC + yC * 8]) && (fromJust gs.worldMatrix.[xC + yC * 8]).player == p.player = pst 		// initial check is necessary!
# updatedPst = updateWorldMatrix (xC,yC) (p.xCord + p.yCord * 8) pst   // update the world matrix
# (game_s , updatedPst2) = getGameState updatedPst					   // get the new gamestate of the world
# isChecked = isUnderCheck p.player game_s.worldMatrix  			   // check if there is a check in the new world matrix
# orig_Pst = {updatedPst2 & ls = gs}								   // returning back the new gamestate to the old one 
| isChecked = orig_Pst												   // if there is a check in the new world matrix, then it is an invalid move.
| isNothing gs.worldMatrix.[xC + yC * 8] = case highlight of
											  True = BackWardMovesAlongAntiDiagonal highlight (xC-1) (yC-1) p {orig_Pst & io = appWindowPicture (gs.windowId) ((hiliteAt point tile)) io , ls = {gs & validMoves = updateBool (xC + yC * 8) gs.validMoves}}  
											  False = BackWardMovesAlongAntiDiagonal highlight (xC-1) (yC-1) p {orig_Pst & ls = {gs & validMoves = updateBool (xC + yC * 8) gs.validMoves}}  
= case highlight of 
					True = {orig_Pst & io = appWindowPicture (gs.windowId) ((hiliteAt point tile)) io , ls = {gs & validMoves = updateBool (xC + yC * 8) gs.validMoves}} // if not the same piece Highlight and stop
					False = {orig_Pst & ls = {gs & validMoves = updateBool (xC + yC * 8) gs.validMoves}} // if not the same piece Highlight and stop
where
	point = {x = xC * TILE_SIZE , y = yC * TILE_SIZE}
	
	
	
	
	
	
	
	
	
	
	
	