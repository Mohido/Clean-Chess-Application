implementation module Util.HighLighting.KnightMoves

import StdEnv, StdIO, Util.Constants, Util.GameOverFunctions , Util.CostumFunctions
from StdFunc import seq
/*
upperRightup upperRightedown  upperLeftup upperLeftdown downRightdown downRightup downLeftdown downLeftup
*/


HighlightKnight :: Bool (*PSt GameState) !Piece -> (*PSt GameState)
HighlightKnight highLight pst=:{ls, io} p = seq [ 
													CheckPoint highLight (xC-1) (yC+2) p, CheckPoint highLight (xC+1) (yC+2) p, // UpperLeftUp, UpperRightUp
													CheckPoint highLight (xC-1) (yC-2) p, CheckPoint highLight (xC+1) (yC-2) p, // DownLeftDown, DownRightDown
													CheckPoint highLight (xC+2) (yC-1) p, CheckPoint highLight (xC-2) (yC-1) p, // UpperLeftDown, UpperRightDown
													CheckPoint highLight (xC+2) (yC+1) p, CheckPoint highLight (xC-2) (yC+1) p  // DownRightUp, DownLeftUp
												] newPst 
where
	xC 	   = (p.xCord) 
	yC 	   = (p.yCord) 
	point  = {x = xC * TILE_SIZE , y = yC * TILE_SIZE}
	newPst = {pst & io = appWindowPicture (ls.windowId) (hiliteAt point tile) io}

/*
* Checking if one Point is highlightable at a time.
*/

CheckPoint :: Bool Int Int !Piece (*PSt GameState) -> (*PSt GameState)
CheckPoint highLight xC yC p pst=:{ls = gs, io} 
| xC < 0  || xC > 7 || yC > 7|| yC < 0 = pst
//# penColour = {r=120, g=192, b=196}
| not (isNothing gs.worldMatrix.[xC + yC * 8]) && (fromJust gs.worldMatrix.[xC + yC * 8]).player == p.player = pst 
# updatedPst = updateWorldMatrix (xC,yC) (p.xCord + p.yCord * 8) pst   // update the world matrix
# (game_s , updatedPst2) = getGameState updatedPst					   // get the new gamestate of the world
# isChecked = isUnderCheck p.player game_s.worldMatrix  			   // check if there is a check in the new world matrix
# orig_Pst = {updatedPst2 & ls = gs}								   // returning back the new gamestate to the old one 
| isChecked = orig_Pst
= case highLight of
				False = case gs.worldMatrix.[xC +yC *8] of 
						Nothing = {orig_Pst & ls = {gs & validMoves = updateBool (xC + yC * 8) gs.validMoves}}
						Just piece = case (piece.player == p.player) of
												True = orig_Pst
												False= {orig_Pst & ls = {gs & validMoves = updateBool (xC + yC * 8) gs.validMoves}}
				True = case gs.worldMatrix.[xC +yC *8] of 
						Nothing = {orig_Pst & 
									io = appWindowPicture (gs.windowId) (hiliteAt point tile) io, 
									ls = {gs & validMoves = updateBool (xC + yC * 8) gs.validMoves}}
						Just piece = case (piece.player == p.player) of
												True = orig_Pst
												False= {orig_Pst & io = appWindowPicture (gs.windowId) (hiliteAt point tile) io, 
									 						ls = {gs & validMoves = updateBool (xC + yC * 8) gs.validMoves}}
where
	point = {x = xC * TILE_SIZE , y = yC * TILE_SIZE}