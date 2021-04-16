implementation module Util.HighLighting.KnightMoves

import StdEnv, StdIO, Util.Constants
from StdFunc import seq
/*
upperRightup upperRightedown  upperLeftup upperLeftdown downRightdown downRightup downLeftdown downLeftup
*/


HighlightKnight :: (*PSt GameState) !Piece -> (*PSt GameState)
HighlightKnight pst=:{ls, io} p = seq [ CheckPoint(xC-1) (yC+2) p, CheckPoint(xC+1) (yC+2) p, // UpperLeftUp, UpperRightUp
										CheckPoint(xC-1) (yC-2) p, CheckPoint(xC+1) (yC-2) p, // DownLeftDown, DownRightDown
										CheckPoint(xC+2) (yC-1) p, CheckPoint(xC-2) (yC-1) p, // UpperLeftDown, UpperRightDown
										CheckPoint(xC+2) (yC+1) p, CheckPoint(xC-2) (yC+1) p] newPst // DownRightUp, DownLeftUp
where
	xC 	   = (p.xCord) 
	yC 	   = (p.yCord) 
	point = {x = xC * TILE_SIZE , y = yC * TILE_SIZE}
	newPst     = {pst & io = appWindowPicture (ls.windowId) (hiliteAt point tile) io}

/*
* Checking if one Point is highlightable at a time.
*/

CheckPoint :: Int Int !Piece (*PSt GameState) -> (*PSt GameState)
CheckPoint xC yC p pst=:{ls, io} 
| xC < 0  || xC > 7 || yC > 7|| yC < 0 = pst
= case ls.worldMatrix.[xC +yC *8] of 
	Nothing = {pst & io = appWindowPicture (ls.windowId) (hiliteAt point tile) io}
	Just piece = case (piece.player == p.player) of
					True = pst
					False= {pst & io = appWindowPicture (ls.windowId) (hiliteAt point tile) io}
where
	point = {x = xC * TILE_SIZE , y = yC * TILE_SIZE}