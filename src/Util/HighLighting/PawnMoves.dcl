definition module Util.HighLighting.PawnMoves 

import StdEnv, StdIO, Util.Constants
from StdFunc import seq


								
/*A bit ugly and needs more polishing but this works well for now.*/
highLightPawn :: (*PSt GameState) !Piece -> (*PSt GameState)

/*One big function to manage moving forward for both white and black pawn pieces.*/
moveForwardPawn :: Int Int Int !Piece (*PSt GameState)-> (*PSt GameState)

moveForwardPawnAux_b :: Int Int !Piece (*PSt GameState)-> (*PSt GameState)

moveForwardPawnAux_w :: Int Int !Piece (*PSt GameState)-> (*PSt GameState)
/*Moving diagonally with a pawn is quite straight forward.*/
moveDiagonallyPawn :: Int Int !Piece (*PSt GameState)-> (*PSt GameState)
