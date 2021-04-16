
definition module Util.HighLighting.BishopMoves
import StdEnv, StdIO, Util.Constants
from StdFunc import seq



HighlightBishop :: (*PSt GameState) !Piece -> (*PSt GameState)


ForwardMovesAlongMainDiagonal :: Int Int !Piece (*PSt GameState) -> (*PSt GameState)


BackWardMovesAlongMainDiagonal :: Int Int !Piece (*PSt GameState) -> (*PSt GameState)


ForwardMovesAlongAntiDiagonal :: Int Int !Piece (*PSt GameState)-> (*PSt GameState)

 
BackWardMovesAlongAntiDiagonal :: Int Int !Piece (*PSt GameState)-> (*PSt GameState)