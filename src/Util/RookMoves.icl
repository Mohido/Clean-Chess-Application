implementation module Util.RookMoves
import StdEnv, StdIO, Util.Constants, StdFunc




highliteRook :: (*PSt GameState) !Piece -> (*PSt GameState)
highliteRook pst=:{ls, io} p = seq [goLeftRook (xC-1) yC p, goRightRook (xC+1) yC p, goForwardRook xC (yC+1) p, goBackwardRook xC (yC-1) p] newPst
where
	xC 	   = (p.xCord) 
	yC 	   = (p.yCord) 
	point = {x = xC * TILE_SIZE , y = yC * TILE_SIZE}
	newPst     = {pst & io = appWindowPicture (ls.windowId) (hiliteAt point tile) io}




goLeftRook :: Int Int !Piece (*PSt GameState) -> (*PSt GameState)
goLeftRook xC yC p pst=:{ls, io} 
| xC < 0 = pst
= case ls.worldMatrix.[xC + yC * 8] of 
	Nothing = goLeftRook (xC-1) yC p {pst & io = appWindowPicture (ls.windowId) (hiliteAt point tile) io}  
	Just piece = case (piece.player == p.player) of
					False = {pst & io = appWindowPicture (ls.windowId) (hiliteAt point tile) io} // if not the same piece Highlight and stop
					True = pst
where
	point = {x = xC * TILE_SIZE , y = yC * TILE_SIZE}
					


goRightRook :: Int Int !Piece (*PSt GameState) -> (*PSt GameState)
goRightRook xC yC p pst=:{ls, io}
| xC > 7 = pst
= case ls.worldMatrix.[xC + yC * 8] of 
	Nothing = goRightRook (xC+1) yC p {pst & io = appWindowPicture (ls.windowId) (hiliteAt point tile) io}  
	Just piece = case (piece.player == p.player) of 
					False = {pst & io = appWindowPicture (ls.windowId) (hiliteAt point tile) io} // if not the same piece Highlight and stop
					True = pst
where
	point = {x = xC * TILE_SIZE , y = yC * TILE_SIZE}





goForwardRook :: Int Int !Piece (*PSt GameState)-> (*PSt GameState)
goForwardRook xC yC p pst=:{ls, io}
| yC > 7 = pst
= case ls.worldMatrix.[xC + yC * 8] of 
	Nothing = goForwardRook xC (yC+1) p {pst & io = appWindowPicture (ls.windowId) (hiliteAt point tile) io}  
	Just piece = case (piece.player == p.player) of 
					False = {pst & io = appWindowPicture (ls.windowId) (hiliteAt point tile) io} // if not the same piece Highlight and stop
					True = pst
where
	point = {x = xC * TILE_SIZE , y = yC * TILE_SIZE}



goBackwardRook :: Int Int !Piece (*PSt GameState)-> (*PSt GameState)
goBackwardRook xC yC p pst=:{ls, io} 
| yC < 0 = pst
= case ls.worldMatrix.[xC + yC * 8] of 
	Nothing = goBackwardRook xC (yC-1) p {pst & io = appWindowPicture (ls.windowId) (hiliteAt point tile) io}  
	Just piece = case (piece.player == p.player) of 
					False = {pst & io = appWindowPicture (ls.windowId) (hiliteAt point tile) io} // if not the same piece Highlight and stop
					True = pst
where
	point = {x = xC * TILE_SIZE , y = yC * TILE_SIZE}									














