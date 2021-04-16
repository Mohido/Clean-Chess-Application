
definition module Util.BishopMoves
import StdEnv, StdIO, StdDebug, Util.Constants, StdFunc



HighlightBishop :: (*PSt GameState) !Piece -> (*PSt GameState)


ForwardMovesAlongMainDiagonal :: Int Int !Piece (*PSt GameState) -> (*PSt GameState)


BackWardMovesAlongMainDiagonal :: Int Int !Piece (*PSt GameState) -> (*PSt GameState)


ForwardMovesAlongAntiDiagonal :: Int Int !Piece (*PSt GameState)-> (*PSt GameState)

 
BackWardMovesAlongAntiDiagonal :: Int Int !Piece (*PSt GameState)-> (*PSt GameState)