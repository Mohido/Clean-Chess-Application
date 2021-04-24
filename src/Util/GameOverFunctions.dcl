definition module Util.GameOverFunctions

import StdEnv, StdIO, Util.Constants


//Returns True if the indexedPLayer king is under check 
isUnderCheck :: PieceColour Board -> Bool

//Get Pieces that are doing a check on the king of the given index player
getCriticalPieces :: PieceColour Board -> [Piece]