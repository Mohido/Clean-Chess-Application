implementation module Util.HighLighting.QueenMoves

import StdEnv, StdIO, Util.Constants, Util.HighLighting.BishopMoves, Util.HighLighting.RookMoves
from StdFunc import seq


HighlightQueen :: (*PSt GameState) !Piece -> (*PSt GameState)
HighlightQueen pst=:{ls, io} p = seq [goLeftRook True (xC-1) yC p , goRightRook True (xC+1) yC p , goForwardRook True xC (yC+1) p , goBackwardRook True xC (yC-1) p, ForwardMovesAlongMainDiagonal (xC-1) (yC+1) p, BackWardMovesAlongMainDiagonal (xC+1) (yC-1) p, ForwardMovesAlongAntiDiagonal (xC+1) (yC+1) p, BackWardMovesAlongAntiDiagonal (xC-1) (yC-1) p] newPst
where
	xC 	   = (p.xCord) 
	yC 	   = (p.yCord) 
	point = {x = xC * TILE_SIZE , y = yC * TILE_SIZE}
	newPst     = {pst & io = appWindowPicture (ls.windowId) (hiliteAt point tile) io}