definition module Util.HighLighting.QueenMoves

import StdEnv, StdIO, Util.Constants, Util.HighLighting.BishopMoves, Util.HighLighting.RookMoves
from StdFunc import seq



HighlightQueen :: Bool (*PSt GameState) !Piece -> (*PSt GameState)