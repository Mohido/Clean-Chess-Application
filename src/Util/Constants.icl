implementation module Util.Constants

import StdEnv, StdIO
initMoves :: {#Bool}
initMoves = {False,False,False,False,False,False,False,False,
			 False,False,False,False,False,False,False,False,
			 False,False,False,False,False,False,False,False,
			 False,False,False,False,False,False,False,False,
			 False,False,False,False,False,False,False,False,
			 False,False,False,False,False,False,False,False,
			 False,False,False,False,False,False,False,False,
			 False,False,False,False,False,False,False,False}

updateBool:: Int {#Bool} -> {#Bool}
updateBool int p = {(\x |x == int = (not a) = a) b \\ a<-:p & b<-[0..]}

initPlayers :: {Player}
initPlayers = {
				{colour=WhitePiece, castleLeft= True,castleRight=True},
				{colour=BlackPiece, castleLeft= True,castleRight=True}
			  }


move :: SoundSample
move = { soundid = SND_JUMP, soundfile = "./Util/SOUNDS/JUMP.WAV", soundbuffers =  1 }

instance == ChessType
where
	(==) Rook Rook = True
	(==) Knight Knight = True
	(==) Bishop Bishop = True
	(==) Queen Queen = True
	(==) Pawn Pawn = True
	(==) NotPiece NotPiece = True
	(==) King King = True
	(==) _ _ = False
	

instance == PieceColour
where 
	(==) BlackPiece BlackPiece = True
	(==) WhitePiece WhitePiece = True
	(==) _ _ = False
	
instance == RGBColour
where
	(==) x y = x.r == y.r && x.b == y.b && x.g == y.g
	(==) _ _ = False