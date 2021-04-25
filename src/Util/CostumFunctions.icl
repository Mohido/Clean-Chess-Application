implementation module Util.CostumFunctions

import StdEnv, StdIO, Util.Constants, Util.Rendering
import clCCall_12
import	ostoolbox
seqArray :: !{*s -> *s} *s -> *s
seqArray arr param = auxSeqArray arr param 0
where
	auxSeqArray :: !{*s -> *s} *s Int -> *s
	auxSeqArray arr param ind
	| ind >= size (arr) = param
	#! param = (arr.[ind] param)
	= auxSeqArray arr param (ind + 1)

	

/// Update Arry function because apparently we cant use the innate function
updateWorldMatrix :: (Int,Int) Int (*PSt GameState) -> (*PSt GameState)
updateWorldMatrix (move,to) originalPos pst=:{ls,io} = switched
where
	newPiece	 = updatePiece move to ls.selectedPiece 
	moveTo   	 = move + to * 8
	movedPiece   = {pst & ls.worldMatrix = {(\x |x == moveTo = newPiece = a) i \\ a<-:ls.worldMatrix & i<-[0..] }}
	switched     = {movedPiece & ls.worldMatrix = {(\x |x == originalPos = Nothing = a) i \\ a<-:movedPiece.ls.worldMatrix & i<-[0..] }}


//*Takes two coordinates and updates the piece's coordinates accordingly*/
updatePiece :: Int Int !(Maybe Piece) -> !(Maybe Piece)
updatePiece xC yC Nothing = Nothing
updatePiece xC yC (Just p) = Just {p & xCord = xC , yCord = yC}
 





/*
printPieces :: (*PSt GameState) -> ((*PSt GameState),String) 
printPieces pst=:{ls,io} = (pst,prints [a \\ a<-:ls.worldMatrix])

prints []    = "\n"
prints [x:xs]
|(isNothing x) = ".\n"+++ prints xs
=toString (fromJust x) +++ prints xs*/









