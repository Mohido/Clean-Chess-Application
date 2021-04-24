implementation module Util.HighLighting.QueenMoves

import StdEnv, StdIO, Util.Constants, Util.HighLighting.BishopMoves, Util.HighLighting.RookMoves
from StdFunc import seq


HighlightQueen :: Bool (*PSt GameState) !Piece -> (*PSt GameState)
HighlightQueen highlight pst=:{ls, io} p = seq [goLeftRook highlight (xC-1) yC p ,
									  goRightRook highlight (xC+1) yC p ,
									  goForwardRook highlight xC (yC+1) p ,
									  goBackwardRook highlight xC (yC-1) p,
									  ForwardMovesAlongMainDiagonal highlight (xC-1) (yC+1) p,
									  BackWardMovesAlongMainDiagonal highlight (xC+1) (yC-1) p,
									  ForwardMovesAlongAntiDiagonal highlight (xC+1) (yC+1) p, 
									  BackWardMovesAlongAntiDiagonal highlight (xC-1) (yC-1) p] newPst
where
	xC 	   = (p.xCord) 
	yC 	   = (p.yCord) 
	point = {x = xC * TILE_SIZE , y = yC * TILE_SIZE}
	newPst     = case highlight of
					True = {pst & io = appWindowPicture (ls.windowId) (hiliteAt point tile) io} 
					False = pst
