definition module Util.CostumFunctions

import StdEnv, StdIO, Util.Constants


/// Applying a recursive sequence of functions on unique types from an array.
seqArray :: !{*s -> *s} *s -> *s

/// Update Arry function because apparently we cant use the innate function
updateWorldMatrix :: (Int,Int) Int (*PSt GameState) -> (*PSt GameState)

//*Takes two coordinates and updates the piece's coordinates accordingly*/
updatePiece :: Int Int !(Maybe Piece) -> !(Maybe Piece)

