implementation module Util.HighLighting.KingMoves
import StdEnv, StdIO, Util.Constants
from StdFunc import seq



HighlightKing :: (*PSt GameState) !Piece -> (*PSt GameState)
HighlightKing pst=:{ls, io} p = seq [KingValidMoves (xC-1) yC p, KingValidMoves (xC+1) yC p,KingValidMoves xC (yC+1) p, 
	KingValidMoves xC (yC-1) p, KingValidMoves (xC+1) (yC+1) p,KingValidMoves (xC+1) (yC-1) p,KingValidMoves (xC-1) (yC+1) p,
	KingValidMoves (xC-1) (yC-1) p, KingSideCastle (xC-1) yC p, QueenSideCastle (xC+1) yC p] newPst
where
	xC 	   = (p.xCord) 
	yC 	   = (p.yCord) 
	point = {x = xC * TILE_SIZE , y = yC * TILE_SIZE}
	newPst     = {pst & io = appWindowPicture (ls.windowId) (hiliteAt point tile) io}


//***************************************************************************************************************
//NOTE: Following conditoin must be included in the first guard in order to complete the highlight feature; 
// (CheckMate)
// left for the end of the game logic implementation
//***************************************************************************************************************

KingValidMoves :: Int Int !Piece (*PSt GameState) -> (*PSt GameState)
KingValidMoves xCoordinate yCoordinate clickedPiece pst=:{ls, io} 
| xCoordinate < 0 || xCoordinate > 7 || yCoordinate < 0 || yCoordinate > 7 = pst 
= case ls.worldMatrix.[xCoordinate + yCoordinate * 8] of 
	Nothing = {pst & io = appWindowPicture (ls.windowId) (hiliteAt point tile) io}  
	Just piece = case (piece.player == clickedPiece.player) of
					False = {pst & io = appWindowPicture (ls.windowId) (hiliteAt point tile) io}
					True = pst
where
	point = {x = xCoordinate * TILE_SIZE , y = yCoordinate * TILE_SIZE} 



  
//***************************************************************************************************************
//NOTE: Following conditoins must be included in the first guard in order to complete the highlight feature; 
// (King has Changed it's positon and comes back to its original postion) OR (Opponent's Check) OR (CheckMate)
// The tiles must not be highligted if either of the above conditions are true
// left for the end of the game logic implementation; Same goes for the queen side castling
//***************************************************************************************************************

kingSideCastleAux :: Int Int (*PSt GameState) -> (*PSt GameState)
kingSideCastleAux xC yC pst=:{ls, io} 
| xC <= 0 = pst 
= kingSideCastleAux (xC - 1) yC {pst & io = appWindowPicture (ls.windowId) (hiliteAt {x = xC * TILE_SIZE , y = yC * TILE_SIZE} tile) io}


KingSideCastle :: Int Int !Piece (*PSt GameState) -> (*PSt GameState)
KingSideCastle xCoordinate yCoordinate clickedPiece pst=:{ls, io} 
| xCoordinate <> 2 && ( yCoordinate <> 0 || yCoordinate <> 7)  = pst //if king is not already on it's orignal positon, we just return the current pst
= case ls.worldMatrix.[xCoordinate + yCoordinate * 8] of
	Nothing = case ls.worldMatrix.[(xCoordinate-1) + yCoordinate * 8] of 
				Nothing = case ls.worldMatrix.[(xCoordinate - 2) + yCoordinate * 8] of 
							Nothing = pst
							Just piece = case(piece.player == clickedPiece.player) of
											False = pst 
											True = case (piece.type == Rook) of
													False = pst 
													True = kingSideCastleAux xCoordinate yCoordinate pst
				Just piece = pst 
	Just piece = pst




QueenSideCastleAux :: Int Int (*PSt GameState) -> (*PSt GameState)
QueenSideCastleAux xC yC pst=:{ls, io}
| xC >= 7 = pst 
= QueenSideCastleAux (xC - 1) yC {pst & io = appWindowPicture (ls.windowId) (hiliteAt {x = xC * TILE_SIZE , y = yC * TILE_SIZE} tile) io}

QueenSideCastle :: Int Int !Piece (*PSt GameState) -> (*PSt GameState)
QueenSideCastle xCoordinate yCoordinate clickedPiece pst=:{ls, io}
| xCoordinate <> 4 && ( yCoordinate <> 0 || yCoordinate <> 7)  = pst //if king is not already on it's orignal positon, we just return the current pst
= case ls.worldMatrix.[xCoordinate + yCoordinate * 8] of
	Nothing = case ls.worldMatrix.[(xCoordinate+1) + yCoordinate * 8] of
				Nothing = case ls.worldMatrix.[(xCoordinate+2) + yCoordinate * 8] of
							Nothing = case ls.worldMatrix.[(xCoordinate+3) + yCoordinate * 8] of
										Nothing = pst
										Just piece = case(piece.player == clickedPiece.player) of
														False = pst 
														True = case (piece.type == Rook) of
																False = pst 
																True = QueenSideCastleAux xCoordinate yCoordinate pst
							Just piece = pst 
				Just piece = pst 
	Just piece = pst

