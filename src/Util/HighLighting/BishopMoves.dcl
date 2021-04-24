
definition module Util.HighLighting.BishopMoves
import StdEnv, StdIO, Util.Constants
from StdFunc import seq



HighlightBishop :: Bool (*PSt GameState) !Piece -> (*PSt GameState)


ForwardMovesAlongMainDiagonal :: Bool Int Int !Piece (*PSt GameState) -> (*PSt GameState)


BackWardMovesAlongMainDiagonal :: Bool Int Int !Piece (*PSt GameState) -> (*PSt GameState)


ForwardMovesAlongAntiDiagonal :: Bool Int Int !Piece (*PSt GameState)-> (*PSt GameState)

 
BackWardMovesAlongAntiDiagonal :: Bool Int Int !Piece (*PSt GameState)-> (*PSt GameState)