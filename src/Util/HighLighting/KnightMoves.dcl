definition module Util.HighLighting.KnightMoves

import StdEnv, StdIO,Util.Constants
from StdFunc import seq

/*MotherFunction that redirects to the checkpoint*/

HighlightKnight :: Bool (*PSt GameState) !Piece -> (*PSt GameState)


/*
* Checking if one Point is highlightable at a time.
*/

CheckPoint :: Bool Int Int !Piece (*PSt GameState) -> (*PSt GameState)