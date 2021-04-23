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
	
instance == Piece
where
	(==) p1 p2 = ( p1.xCord == p2.xCord && p1.yCord == p2.yCord && 
				   p1.player == p2.player && p1.type == p2.type )
				   
instance toString ChessType 
where
	toString Rook = "Rook"
	toString Knight = "Knight" 
	toString Bishop = "Bishop" 
	toString Queen = "Queen" 
	toString King = "King" 
	toString Pawn = "Pawn" 
	toString NotPiece = "Not a Piece" 
	
	
instance toString PieceColour 
where
	toString BlackPiece = "Black Piece"
	toString WhitePiece = "White Piece" 
	toString ColourError = "Colour Error"
	
instance toString Piece 
where
	toString p = "Piece: Cords = (" +++ toString p.xCord 
    					   +++ ", " +++ toString p.yCord +++ ") \n"
    					   +++ "Color: " +++ toString p.player
    					   +++ "\nType: " +++ toString p.type 