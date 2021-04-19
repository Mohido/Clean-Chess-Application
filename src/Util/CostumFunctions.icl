implementation module Util.CostumFunctions

import StdEnv, StdIO, Util.Constants, Util.Rendering

seqArray :: !{*s -> *s} *s -> *s
seqArray arr param = auxSeqArray arr param 0
where
	auxSeqArray :: !{*s -> *s} *s Int -> *s
	auxSeqArray arr param ind
	| ind >= size (arr) = param
	#! param = (arr.[ind] param)
	= auxSeqArray arr param (ind + 1)

	
/// Update Arry function because apparently we cant use the innate function
updateWorldMatrix :: (Int,Int) Int !(Maybe Piece) GameState -> GameState
updateWorldMatrix (move,to) originalPos m gs = switched
where
	s = updatePiece move to m 
	moveTo = move + to * 8
	movedPiece = {gs & worldMatrix = {(\x |x == moveTo = s = a) i \\ a<-:gs.worldMatrix & i<-[0..] }}
	switched = {movedPiece & worldMatrix = {(\x |x == originalPos = Nothing = a) i \\ a<-:movedPiece.worldMatrix & i<-[0..] }}


	

