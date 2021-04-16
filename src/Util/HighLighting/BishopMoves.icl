implementation module Util.HighLighting.BishopMoves
import StdEnv, StdIO, Util.Constants
from StdFunc import seq

//**************************************************************************
// NOTE: functions ForwardMovesAlongMainDiagonal, BackWardMovesAlongMainDiagonal,
// ForwardMovesAlongAntiDiagonal, and BackWardMovesAlongAntiDiagonal must include the following conditions include in the first gaurd: 
//
// isCheckMate
// It should also deal with whether the king is under check: which can only be done when done with the game logic implementation
//******************************************************************************


HighlightBishop :: (*PSt GameState) !Piece -> (*PSt GameState)
HighlightBishop pst=:{ls, io} p = seq [ForwardMovesAlongMainDiagonal (xC-1) (yC+1) p, BackWardMovesAlongMainDiagonal (xC+1) (yC-1) p, ForwardMovesAlongAntiDiagonal (xC+1) (yC+1) p, BackWardMovesAlongAntiDiagonal (xC-1) (yC-1) p] newPst
where
	xC 	   = (p.xCord) 
	yC 	   = (p.yCord) 
	point = {x = xC * TILE_SIZE , y = yC * TILE_SIZE}
	newPst     = {pst & io = appWindowPicture (ls.windowId) (hiliteAt point tile) io}



//one-forward-one-to-the-left
ForwardMovesAlongMainDiagonal :: Int Int !Piece (*PSt GameState) -> (*PSt GameState)
ForwardMovesAlongMainDiagonal xC yC p pst=:{ls, io} 
| xC < 0 || yC > 7 = pst
= case ls.worldMatrix.[xC + yC * 8] of 
	Nothing = ForwardMovesAlongMainDiagonal (xC-1) (yC+1) p {pst & io = appWindowPicture (ls.windowId) (hiliteAt point tile) io}  
	Just piece = case (piece.player == p.player) of
					False = {pst & io = appWindowPicture (ls.windowId) (hiliteAt point tile) io} 
					True = pst
where
	point = {x = xC * TILE_SIZE , y = yC * TILE_SIZE}
					

//one-back-ward-one-to-the-right
BackWardMovesAlongMainDiagonal :: Int Int !Piece (*PSt GameState) -> (*PSt GameState)
BackWardMovesAlongMainDiagonal xC yC p pst=:{ls, io}
| xC > 7 || yC < 0 = pst || 
= case ls.worldMatrix.[xC + yC * 8] of 
	Nothing = BackWardMovesAlongMainDiagonal (xC+1) (yC-1) p {pst & io = appWindowPicture (ls.windowId) (hiliteAt point tile) io}  
	Just piece = case (piece.player == p.player) of 
					False = {pst & io = appWindowPicture (ls.windowId) (hiliteAt point tile) io} // if not the same piece Highlight and stop
					True = pst
where
	point = {x = xC * TILE_SIZE , y = yC * TILE_SIZE}




//one-forward-one-to-the-right
ForwardMovesAlongAntiDiagonal:: Int Int !Piece (*PSt GameState)-> (*PSt GameState)
ForwardMovesAlongAntiDiagonal xC yC p pst=:{ls, io}
| yC > 7 || xC > 7 = pst
= case ls.worldMatrix.[xC + yC * 8] of 
	Nothing = ForwardMovesAlongAntiDiagonal (xC + 1) (yC+1) p {pst & io = appWindowPicture (ls.windowId) (hiliteAt point tile) io}  
	Just piece = case (piece.player == p.player) of 
					False = {pst & io = appWindowPicture (ls.windowId) (hiliteAt point tile) io} // if not the same piece Highlight and stop
					True = pst
where
	point = {x = xC * TILE_SIZE , y = yC * TILE_SIZE}


//one-backward-one-to-the-left
BackWardMovesAlongAntiDiagonal :: Int Int !Piece (*PSt GameState)-> (*PSt GameState)
BackWardMovesAlongAntiDiagonal xC yC p pst=:{ls, io} 
| yC < 0 || xC < 0 = pst
= case ls.worldMatrix.[xC + yC * 8] of 
	Nothing = BackWardMovesAlongAntiDiagonal (xC-1) (yC-1) p {pst & io = appWindowPicture (ls.windowId) (hiliteAt point tile) io}  
	Just piece = case (piece.player == p.player) of 
					False = {pst & io = appWindowPicture (ls.windowId) (hiliteAt point tile) io} // if not the same piece Highlight and stop
					True = pst
where
	point = {x = xC * TILE_SIZE , y = yC * TILE_SIZE}