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
	newPst     = if True {pst & io = appWindowPicture (ls.windowId) (hiliteAt point tile) io} pst



goLeftRook :: Bool Int Int !Piece (*PSt GameState)  -> (*PSt GameState)
goLeftRook highlight xC yC p pst=:{ls, io} 
| xC < 0 = pst
# penColour = {r=120, g=192, b=196}
//# pst = updateWorldMatrix (xC,yC) p pst ///Moving the piece toward the destination.
//| isUnderCheck 
| isNothing ls.worldMatrix.[xC + yC * 8] = case highlight of 
												True = goLeftRook highlight (xC-1) yC p {pst & io = appWindowPicture (ls.windowId) ((hiliteAt point tile)) io , ls = {ls & validMoves = updateBool (xC + yC * 8) ls.validMoves}}
												False = goLeftRook highlight (xC-1) yC p {pst & ls = {ls & validMoves = updateBool (xC + yC * 8) ls.validMoves}}
# piece = fromJust ls.worldMatrix.[xC + yC * 8]
= case (piece.player == p.player) of
					False = case highlight of
							True = {pst & io = appWindowPicture (ls.windowId) ((hiliteAt point tile)) io , ls = {ls & validMoves = updateBool (xC + yC * 8) ls.validMoves}} // if not the same piece Highlight and stop
							False = {pst & ls = {ls & validMoves = updateBool (xC + yC * 8) ls.validMoves}} // if not the same piece Highlight and stop
					True = pst
where
	point = {x = xC * TILE_SIZE , y = yC * TILE_SIZE}
					


goRightRook :: Bool Int Int !Piece (*PSt GameState)  -> (*PSt GameState)
goRightRook highlight xC yC p pst=:{ls, io} 
| xC > 7 = pst
| isNothing ls.worldMatrix.[xC + yC * 8] =  case highlight of 
               									True = goRightRook highlight (xC+1) yC p {pst & io = appWindowPicture (ls.windowId) ((hiliteAt point tile)) io , ls = {ls & validMoves = updateBool (xC + yC * 8) ls.validMoves}}  
               									False = goRightRook highlight (xC+1) yC p {pst & ls = {ls & validMoves = updateBool (xC + yC * 8) ls.validMoves}}  
# piece = fromJust ls.worldMatrix.[xC + yC * 8]
=   case (piece.player == p.player) of 
					False = case highlight of 
								True = {pst & io = appWindowPicture (ls.windowId) ((hiliteAt point tile)) io , ls = {ls & validMoves = updateBool (xC + yC * 8) ls.validMoves}} // if not the same piece Highlight and stop
								False = {pst & ls = {ls & validMoves = updateBool (xC + yC * 8) ls.validMoves}} // if not the same piece Highlight and stop
					True = pst
where
	point = {x = xC * TILE_SIZE , y = yC * TILE_SIZE}





goForwardRook :: Bool Int Int !Piece (*PSt GameState)  -> (*PSt GameState)
goForwardRook highlight xC yC p pst=:{ls, io} 
| yC > 7 = pst
| isNothing ls.worldMatrix.[xC + yC * 8] = case highlight of 
												True = goForwardRook highlight xC (yC+1) p {pst & io = appWindowPicture (ls.windowId) ((hiliteAt point tile)) io , ls = {ls & validMoves = updateBool (xC + yC * 8) ls.validMoves}}  
												False = goForwardRook highlight xC (yC+1) p {pst & ls = {ls & validMoves = updateBool (xC + yC * 8) ls.validMoves}}  
# piece = fromJust ls.worldMatrix.[xC + yC * 8]		
=  case (piece.player == p.player) of 
					False = case highlight of
								True = {pst & io = appWindowPicture (ls.windowId) ((hiliteAt point tile)) io , ls = {ls & validMoves = updateBool (xC + yC * 8) ls.validMoves}} // if not the same piece Highlight and stop
								False = {pst & ls = {ls & validMoves = updateBool (xC + yC * 8) ls.validMoves}} // if not the same piece Highlight and stop
					True = pst
where
	point = {x = xC * TILE_SIZE , y = yC * TILE_SIZE}



goBackwardRook :: Bool Int Int !Piece (*PSt GameState) -> (*PSt GameState)
goBackwardRook highlight xC yC p pst=:{ls, io}  
| yC < 0 = pst 
| isNothing ls.worldMatrix.[xC + yC * 8] = case highlight of 
												True = goBackwardRook highlight xC (yC-1) p {pst & io = appWindowPicture (ls.windowId) ((hiliteAt point tile)) io , ls = {ls & validMoves = updateBool (xC + yC * 8) ls.validMoves}}  
												False = goBackwardRook highlight xC (yC-1) p {pst & ls = {ls & validMoves = updateBool (xC + yC * 8) ls.validMoves}}  
# piece = fromJust ls.worldMatrix.[xC + yC * 8]
= case (piece.player == p.player) of 
					False = case highlight of 
							True = {pst & io = appWindowPicture (ls.windowId) ((hiliteAt point tile)) io , ls = {ls & validMoves = updateBool (xC + yC * 8) ls.validMoves}} // if not the same piece Highlight and stop
							False = {pst & ls = {ls & validMoves = updateBool (xC + yC * 8) ls.validMoves}} // if not the same piece Highlight and stop
					True = pst
where
	point = {x = xC * TILE_SIZE , y = yC * TILE_SIZE}									












