definition module Util.HighLighting.PawnMoves 

import StdEnv, StdIO, Util.Constants
from StdFunc import seq


								
/*A bit ugly and needs more polishing but this works well for now.*/
highLightPawn :: Bool (*PSt GameState) !Piece -> (*PSt GameState)

/*One big function to manage moving forward for both white and black pawn pieces.*/
moveForwardPawn :: Bool Int Int !Piece (*PSt GameState)-> (*PSt GameState)

moveForwardPawnAux_b :: Bool Int Int !Piece (*PSt GameState)-> (*PSt GameState)

moveForwardPawnAux_w :: Bool Int Int !Piece (*PSt GameState)-> (*PSt GameState)
/*Moving diagonally with a pawn is quite straight forward.*/
moveDiagonallyPawn :: Bool Int Int !Piece (*PSt GameState)-> (*PSt GameState)
