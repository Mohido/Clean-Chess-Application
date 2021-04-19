implementation module Util.HighLighting.PawnMoves 

import StdEnv, StdIO, Util.Constants
from StdFunc import seq

								
/*A bit ugly and needs more polishing but this works well for now.*/
highLightPawn :: (*PSt GameState) !Piece -> (*PSt GameState)
highLightPawn pst=:{ls, io} p 
|p.player == BlackPiece =  seq [moveForwardPawn xC (yC+1) p, moveDiagonallyPawn (xC+1) (yC+1) p, moveDiagonallyPawn (xC-1) (yC+1) p] newPst
= seq [moveForwardPawn xC (yC-1) p,moveDiagonallyPawn (xC+1) (yC-1) p, moveDiagonallyPawn (xC-1) (yC-1) p] newPst
where
	xC 	   = (p.xCord) 
	yC 	   = (p.yCord) 
	point  = {x = xC * TILE_SIZE , y = yC * TILE_SIZE}
	newPst = {pst & io = appWindowPicture (ls.windowId) (hiliteAt point tile) io}
	
/*One big function to manage moving forward for both white and black pawn pieces.*/
moveForwardPawn :: Int Int !Piece (*PSt GameState)-> (*PSt GameState)
moveForwardPawn xC yC p pst=:{ls, io}
|yC < 0 || yC > 7 = pst
|p.player == BlackPiece = moveForwardPawnAux_b xC yC p pst
	 					= moveForwardPawnAux_w xC yC p pst

moveForwardPawnAux_b :: Int Int !Piece (*PSt GameState)-> (*PSt GameState)
moveForwardPawnAux_b xC yC p pst=:{ls, io}
|(yC == 2) = case ls.worldMatrix.[xC + yC * 8] of //If pawn is at init pos, can move twice
				Nothing = moveForwardPawnAux_b xC (yC+1) p {pst & io = appWindowPicture (ls.windowId) (hiliteAt point tile) io, ls = {ls & validMoves = updateBool (xC + yC * 8) ls.validMoves}} 
				Just piece = case (p.player == piece.player) of
						True = pst
						False= {pst & io = appWindowPicture (ls.windowId) (hiliteAt point tile) io, ls = {ls & validMoves = updateBool (xC + yC * 8) ls.validMoves}}			

|otherwise = case ls.worldMatrix.[xC + yC * 8] of //Otherwise, can only move once
				Nothing = {pst & io = appWindowPicture (ls.windowId) (hiliteAt point tile) io, ls = {ls & validMoves = updateBool (xC + yC * 8) ls.validMoves}}
				Just piece = case (p.player == piece.player) of
						True = pst
						False= {pst & io = appWindowPicture (ls.windowId) (hiliteAt point tile) io, ls = {ls & validMoves = updateBool (xC + yC * 8) ls.validMoves}}
where
	point  = {x = xC * TILE_SIZE , y = yC * TILE_SIZE}

moveForwardPawnAux_w :: Int Int !Piece (*PSt GameState)-> (*PSt GameState)
moveForwardPawnAux_w xC yC p pst=:{ls, io}
|(yC == 5) = case ls.worldMatrix.[xC + yC * 8] of //If pawn is at init pos, can move twice
				Nothing = moveForwardPawnAux_w xC (yC-1) p {pst & io = appWindowPicture (ls.windowId) (hiliteAt point tile) io, ls = {ls & validMoves = updateBool (xC + yC * 8) ls.validMoves}} 
				Just piece = case (p.player == piece.player) of
						True = pst
						False= {pst & io = appWindowPicture (ls.windowId) (hiliteAt point tile) io, ls = {ls & validMoves = updateBool (xC + yC * 8) ls.validMoves}}			

|otherwise = case ls.worldMatrix.[xC + yC * 8] of //Otherwise, can only move once
				Nothing = {pst & io = appWindowPicture (ls.windowId) (hiliteAt point tile) io, ls = {ls & validMoves = updateBool (xC + yC * 8) ls.validMoves}}
				Just piece = case (p.player == piece.player) of
						True = pst
						False= {pst & io = appWindowPicture (ls.windowId) (hiliteAt point tile) io, ls = {ls & validMoves = updateBool (xC + yC * 8) ls.validMoves}}
where
	point  = {x = xC * TILE_SIZE , y = yC * TILE_SIZE}

/*Moving diagonally with a pawn is quite straight forward.*/
moveDiagonallyPawn :: Int Int !Piece (*PSt GameState)-> (*PSt GameState)
moveDiagonallyPawn xC yC p pst=:{ls, io}
|xC <0 ||xC > 7  || yC < 0 || yC > 7 = pst
= case ls.worldMatrix.[xC + yC * 8] of
	Nothing = pst
	Just piece = case (p.player == piece.player) of
				True = pst
				False= {pst & io = appWindowPicture (ls.windowId) (hiliteAt point tile) io, ls = {ls & validMoves = updateBool (xC + yC * 8) ls.validMoves}}
where
	point  = {x = xC * TILE_SIZE , y = yC * TILE_SIZE}
