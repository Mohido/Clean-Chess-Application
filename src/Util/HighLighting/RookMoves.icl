implementation module Util.HighLighting.RookMoves
import StdEnv, StdIO, Util.Constants, StdDebug, Util.CostumFunctions, Util.GameOverFunctions
from StdFunc import seq

highLightRook :: Bool (*PSt GameState) !Piece -> (*PSt GameState)
highLightRook highlight pst=:{ls, io} p = seq [	goLeftRook highlight (xC-1) yC p , 
									 			goRightRook highlight (xC+1) yC p ,
									 			goForwardRook highlight xC (yC+1) p ,
									 			goBackwardRook highlight xC (yC-1) p 
												] newPst
where
	xC 	   = (p.xCord) 
	yC 	   = (p.yCord) 
	point = {x = xC * TILE_SIZE , y = yC * TILE_SIZE}
	newPst     = case highlight of
					True = {pst & io = appWindowPicture (ls.windowId) (hiliteAt point tile) io} 
					False = pst



goLeftRook :: Bool Int Int !Piece (*PSt GameState)  -> (*PSt GameState)
goLeftRook highlight xC yC p pst=:{ls=gs, io} 
| xC < 0 = pst
//# penColour = {r=120, g=192, b=196}
| not (isNothing gs.worldMatrix.[xC + yC * 8]) && (fromJust gs.worldMatrix.[xC + yC * 8]).player == p.player = pst 		// initial check is necessary!
# updatedPst = updateWorldMatrix (xC,yC) (p.xCord + p.yCord * 8) pst   // update the world matrix
# (game_s , updatedPst2) = getGameState updatedPst					   // get the new gamestate of the world
# isChecked = isUnderCheck p.player game_s.worldMatrix  			   // check if there is a check in the new world matrix
# orig_Pst = {updatedPst2 & ls = gs}								   // returning back the new gamestate to the old one 
| isChecked = orig_Pst												   // if there is a check in the new world matrix, then it is an invalid move.
| isNothing gs.worldMatrix.[xC + yC * 8] = case highlight of 
												True = goLeftRook highlight (xC-1) yC p {orig_Pst & io = appWindowPicture (gs.windowId) ((hiliteAt point tile)) io , ls = {gs & validMoves = updateBool (xC + yC * 8) gs.validMoves}}
												False = goLeftRook highlight (xC-1) yC p {orig_Pst & ls = {gs & validMoves = updateBool (xC + yC * 8) gs.validMoves}}
= case highlight of
		True = {orig_Pst & io = appWindowPicture (gs.windowId) ((hiliteAt point tile)) io , ls = {gs & validMoves = updateBool (xC + yC * 8) gs.validMoves}} // if not the same piece Highlight and stop
		False = {orig_Pst & ls = {gs & validMoves = updateBool (xC + yC * 8) gs.validMoves}} // if not the same piece Highlight and stop
where
	point = {x = xC * TILE_SIZE , y = yC * TILE_SIZE}
					


goRightRook :: Bool Int Int !Piece (*PSt GameState)  -> (*PSt GameState)
goRightRook highlight xC yC p pst=:{ls=gs, io} 
| xC > 7 = pst
| not (isNothing gs.worldMatrix.[xC + yC * 8]) && (fromJust gs.worldMatrix.[xC + yC * 8]).player == p.player = pst 		// initial check is necessary!
# updatedPst = updateWorldMatrix (xC,yC) (p.xCord + p.yCord * 8) pst   // update the world matrix
# (game_s , updatedPst2) = getGameState updatedPst					   // get the new gamestate of the world
# isChecked = isUnderCheck p.player game_s.worldMatrix  			   // check if there is a check in the new world matrix
# orig_Pst = {updatedPst2 & ls = gs}								   // returning back the new gamestate to the old one 
| isChecked = orig_Pst												   // if there is a check in the new world matrix, then it is an invalid move.
| isNothing gs.worldMatrix.[xC + yC * 8] =  case highlight of 
               									True = goRightRook highlight (xC+1) yC p {orig_Pst & io = appWindowPicture (gs.windowId) ((hiliteAt point tile)) io , ls = {gs & validMoves = updateBool (xC + yC * 8) gs.validMoves}}  
               									False = goRightRook highlight (xC+1) yC p {orig_Pst & ls = {gs & validMoves = updateBool (xC + yC * 8) gs.validMoves}}  
# piece = fromJust gs.worldMatrix.[xC + yC * 8]
= case highlight of 
		True = {orig_Pst & io = appWindowPicture (gs.windowId) ((hiliteAt point tile)) io , ls = {gs & validMoves = updateBool (xC + yC * 8) gs.validMoves}} // if not the same piece Highlight and stop
		False = {orig_Pst & ls = {gs & validMoves = updateBool (xC + yC * 8) gs.validMoves}} // if not the same piece Highlight and stop
where
	point = {x = xC * TILE_SIZE , y = yC * TILE_SIZE}





goForwardRook :: Bool Int Int !Piece (*PSt GameState)  -> (*PSt GameState)
goForwardRook highlight xC yC p pst=:{ls=gs, io} 
| yC > 7 = pst
| not (isNothing gs.worldMatrix.[xC + yC * 8]) && (fromJust gs.worldMatrix.[xC + yC * 8]).player == p.player = pst 		// initial check is necessary!
# updatedPst = updateWorldMatrix (xC,yC) (p.xCord + p.yCord * 8) pst   // update the world matrix
# (game_s , updatedPst2) = getGameState updatedPst					   // get the new gamestate of the world
# isChecked = isUnderCheck p.player game_s.worldMatrix  			   // check if there is a check in the new world matrix
# orig_Pst = {updatedPst2 & ls = gs}								   // returning back the new gamestate to the old one 
| isChecked = orig_Pst												   // if there is a check in the new world matrix, then it is an invalid move.
| isNothing gs.worldMatrix.[xC + yC * 8] = case highlight of 
												True = goForwardRook highlight xC (yC+1) p {orig_Pst & io = appWindowPicture (gs.windowId) ((hiliteAt point tile)) io , ls = {gs & validMoves = updateBool (xC + yC * 8) gs.validMoves}}  
												False = goForwardRook highlight xC (yC+1) p {orig_Pst & ls = {gs & validMoves = updateBool (xC + yC * 8) gs.validMoves}}  
# piece = fromJust gs.worldMatrix.[xC + yC * 8]		
= case highlight of
	True = {orig_Pst & io = appWindowPicture (gs.windowId) ((hiliteAt point tile)) io , ls = {gs & validMoves = updateBool (xC + yC * 8) gs.validMoves}} // if not the same piece Highlight and stop
	False = {orig_Pst & ls = {gs & validMoves = updateBool (xC + yC * 8) gs.validMoves}} // if not the same piece Highlight and stop
where
	point = {x = xC * TILE_SIZE , y = yC * TILE_SIZE}



goBackwardRook :: Bool Int Int !Piece (*PSt GameState) -> (*PSt GameState)
goBackwardRook highlight xC yC p pst=:{ls=gs, io}  
| yC < 0 = pst 
| not (isNothing gs.worldMatrix.[xC + yC * 8]) && (fromJust gs.worldMatrix.[xC + yC * 8]).player == p.player = pst 		// initial check is necessary!
# updatedPst = updateWorldMatrix (xC,yC) (p.xCord + p.yCord * 8) pst   // update the world matrix
# (game_s , updatedPst2) = getGameState updatedPst					   // get the new gamestate of the world
# isChecked = isUnderCheck p.player game_s.worldMatrix  			   // check if there is a check in the new world matrix
# orig_Pst = {updatedPst2 & ls = gs}								   // returning back the new gamestate to the old one 
| isChecked = orig_Pst												   // if there is a check in the new world matrix, then it is an invalid move.
| isNothing gs.worldMatrix.[xC + yC * 8] = case highlight of 
												True = goBackwardRook highlight xC (yC-1) p {orig_Pst & io = appWindowPicture (gs.windowId) ((hiliteAt point tile)) io , ls = {gs & validMoves = updateBool (xC + yC * 8) gs.validMoves}}  
												False = goBackwardRook highlight xC (yC-1) p {orig_Pst & ls = {gs & validMoves = updateBool (xC + yC * 8) gs.validMoves}}  
# piece = fromJust gs.worldMatrix.[xC + yC * 8]
= case highlight of 
	True = {orig_Pst & io = appWindowPicture (gs.windowId) ((hiliteAt point tile)) io , ls = {gs & validMoves = updateBool (xC + yC * 8) gs.validMoves}} // if not the same piece Highlight and stop
	False = {orig_Pst & ls = {gs & validMoves = updateBool (xC + yC * 8) gs.validMoves}} // if not the same piece Highlight and stop
where
	point = {x = xC * TILE_SIZE , y = yC * TILE_SIZE}									












