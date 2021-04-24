implementation module Util.HighLighting.KingMoves
import StdEnv, StdIO, Util.Constants, Util.GameOverFunctions, Util.CostumFunctions
from StdFunc import seq



HighlightKing :: Bool (*PSt GameState) !Piece -> (*PSt GameState)
HighlightKing highlight pst=:{ls, io} p = seq [	KingValidMoves highlight (xC-1) yC p, 
										KingValidMoves highlight (xC+1) yC p,
										KingValidMoves highlight xC (yC+1) p, 
										KingValidMoves highlight xC (yC-1) p, 
										KingValidMoves highlight (xC+1) (yC+1) p,
										KingValidMoves highlight (xC+1) (yC-1) p,
										KingValidMoves highlight (xC-1) (yC+1) p,
										KingValidMoves highlight (xC-1) (yC-1) p, 
										KingSideCastle highlight (xC+1) yC p, 
										QueenSideCastle highlight (xC-1) yC p
									 ] newPst
where
	xC 	   = (p.xCord) 
	yC 	   = (p.yCord) 
	point = {x = xC * TILE_SIZE , y = yC * TILE_SIZE}
	newPst     = case highlight of
					True = {pst & io = appWindowPicture (ls.windowId) (hiliteAt point tile) io} 
					False = pst


//***************************************************************************************************************
//NOTE: Following conditoin must be included in the first guard in order to complete the highlight feature; 
// (CheckMate)
// left for the end of the game logic implementation
//***************************************************************************************************************

KingValidMoves :: Bool Int Int !Piece (*PSt GameState) -> (*PSt GameState)
KingValidMoves highlight xCoordinate yCoordinate clickedPiece pst=:{ls=gs, io} 
| xCoordinate < 0 || xCoordinate > 7 || yCoordinate < 0 || yCoordinate > 7 = pst 
| not (isNothing gs.worldMatrix.[xCoordinate + yCoordinate * 8]) && (fromJust gs.worldMatrix.[xCoordinate + yCoordinate * 8]).player == clickedPiece.player = pst 		// initial check is necessary!
# updatedPst = updateWorldMatrix (xCoordinate,yCoordinate) (clickedPiece.xCord + clickedPiece.yCord * 8) pst   // update the world matrix
# (game_s , updatedPst2) = getGameState updatedPst					   // get the new gamestate of the world
# isChecked = isUnderCheck clickedPiece.player game_s.worldMatrix  			   // check if there is a check in the new world matrix
# orig_Pst = {updatedPst2 & ls = gs}								   // returning back the new gamestate to the old one 
| isChecked = orig_Pst	
| isNothing gs.worldMatrix.[xCoordinate + yCoordinate * 8] =  case highlight of
																	True = {orig_Pst & io = appWindowPicture (gs.windowId) ((hiliteAt point tile)) io , ls = {gs & validMoves = updateBool (xCoordinate + yCoordinate * 8) gs.validMoves}}  
																	False = {orig_Pst & ls = {gs & validMoves = updateBool (xCoordinate + yCoordinate * 8) gs.validMoves}}  
= case highlight of
					True = {orig_Pst & io = appWindowPicture (gs.windowId) ((hiliteAt point tile)) io , ls = {gs & validMoves = updateBool (xCoordinate + yCoordinate * 8) gs.validMoves}}
					False = {orig_Pst &  ls = {gs & validMoves = updateBool (xCoordinate + yCoordinate * 8) gs.validMoves}}
where
	point = {x = xCoordinate * TILE_SIZE , y = yCoordinate * TILE_SIZE} 



  
//***************************************************************************************************************
//NOTE: Following conditoins must be included in the first guard in order to complete the highlight feature; 
// (King has Changed it's positon and comes back to its original postion) OR (Opponent's Check) OR (CheckMate)
// The tiles must not be highligted if either of the above conditions are true
// left for the end of the game logic implementation; Same goes for the queen side castling
//***************************************************************************************************************



KingSideCastle :: Bool Int Int !Piece (*PSt GameState) -> (*PSt GameState)
KingSideCastle highlight xCoordinate yCoordinate clickedPiece pst=:{ls, io} 
| xCoordinate <> 5 && ( yCoordinate <> 0 || yCoordinate <> 7)  = pst //if king is not already on its orignal positon, we just return the current pst
= case ls.worldMatrix.[xCoordinate + yCoordinate * 8] of
	Nothing = case ls.worldMatrix.[(xCoordinate+1) + yCoordinate * 8] of 
				Nothing = case ls.worldMatrix.[(xCoordinate + 2) + yCoordinate * 8] of 
							Nothing = pst
							Just piece = case (piece.player == clickedPiece.player) of
											False = pst 
											True = case (piece.type == Rook) of
													False = pst 
													True = case highlight of 
														True = {pst & io = appWindowPicture (ls.windowId) (hiliteAt {  x = (xCoordinate + 1) * TILE_SIZE , y = yCoordinate * TILE_SIZE}  tile) io, ls = {ls & validMoves = updateBool ((xCoordinate + 1) + yCoordinate * 8) ls.validMoves}} 
														False = {pst & ls = {ls & validMoves = updateBool ((xCoordinate + 1) + yCoordinate * 8) ls.validMoves}} 
				Just piece = pst	
	Just piece = pst





QueenSideCastle :: Bool Int Int !Piece (*PSt GameState) -> (*PSt GameState)
QueenSideCastle highlight xCoordinate yCoordinate clickedPiece pst=:{ls, io}
| xCoordinate <> 3 && ( yCoordinate <> 0 || yCoordinate <> 7)  = pst //if king is not already on its orignal positon, we just return the current pst
= case ls.worldMatrix.[xCoordinate + yCoordinate * 8] of
	Nothing = case ls.worldMatrix.[(xCoordinate-1) + yCoordinate * 8] of
				Nothing = case ls.worldMatrix.[(xCoordinate-2) + yCoordinate * 8] of
							Nothing = case ls.worldMatrix.[(xCoordinate-3) + yCoordinate * 8] of
										Nothing = pst
										Just piece = case(piece.player == clickedPiece.player) of
														False = pst 
														True = case (piece.type == Rook) of
																False = pst 
																True = case highlight of
																	True = {pst & io = appWindowPicture (ls.windowId) (hiliteAt {  x = (xCoordinate - 1) * TILE_SIZE , y = yCoordinate * TILE_SIZE}  tile) io, ls = {ls & validMoves = updateBool ((xCoordinate - 1) + yCoordinate * 8) ls.validMoves}}
																	False = {pst & ls = {ls & validMoves = updateBool ((xCoordinate - 1) + yCoordinate * 8) ls.validMoves}}
							Just piece = pst 
				Just piece = pst 
	Just piece = pst
	
	
	
	
	
	
	
