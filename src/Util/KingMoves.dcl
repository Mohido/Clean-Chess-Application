definition module Util.KingMoves
import StdEnv, StdIO, Util.Constants, StdFunc



//*********************************************************
// Highlight all the valid moves of King (regardless of colour) 
// Takes Process State, Games State and Piece, and returns Process State
// and Game State
//**********************************************************
HighlightKing :: (*PSt GameState) !Piece -> (*PSt GameState)


//**********************************************************************************
//Check (and highlight) whether there is a valid move on the given point (Int, Int) that is provided when triggered
//So, for left, right, forward, backward, we provide (xC-1,yC), (xC+1, yC), (xC, yC+1), (xC, y-1) respectively 
//and for forward-right : (xC+1, yC+1), forward-left: (xC-1, yC+1), backward-left: (xC-1, xC-1), backward-right: (xC+l, xC-1)
//- where xC and yC are the x and y coordinates of the clicked tile 
//*************************************************************************************


KingValidMoves :: Int Int !Piece (*PSt GameState) -> (*PSt GameState)

//Check (and highlight) whether the king can castle King side
KingSideCastle :: Int Int !Piece (*PSt GameState) -> (*PSt GameState)

//Check (and highlight) whether the king can castle QueenSide side
QueenSideCastle :: Int Int !Piece (*PSt GameState) -> (*PSt GameState)
