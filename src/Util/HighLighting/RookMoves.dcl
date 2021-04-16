definition module Util.HighLighting.RookMoves
import StdEnv, StdIO, StdDebug, Util.Constants


//* The mother function.
highLightRook :: (*PSt GameState) !Piece -> (*PSt GameState)

//----Highlighting the pieces that are left of our selected piece 
goLeftRook :: Int Int !Piece (*PSt GameState) -> (*PSt GameState)

//----Highlighting the pieces that are right of our selected piece 
goRightRook :: Int Int !Piece (*PSt GameState) -> (*PSt GameState)

//----Highlighting the pieces that are in front of our selected piece 
goForwardRook :: Int Int !Piece (*PSt GameState)-> (*PSt GameState)

//----Highlighting the pieces that are behind our selected piece 
goBackwardRook :: Int Int !Piece (*PSt GameState)-> (*PSt GameState)
