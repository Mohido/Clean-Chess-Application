implementation module Util.Event

import StdEnv, StdIO, StdDebug, Util.Constants, Util.Rendering


///____________ Mouse Handling events functions_____________


/* 
* testing:- 
mouseHandler (MouseDown hitPoint _ _) (nil, pst=:{ls=gs}) = = trace_n ( toString xCord +++ " " +++ toString yCord  +++ " ." +++ printingPiece gs.selectedPiece) (nil, newPST)
*/

mouseHandler :: MouseState (.ls, *PSt GameState) -> (.ls,*PSt GameState)
mouseHandler (MouseDown hitPoint _ _) (nil, pst=:{ls=gs, io}) = (nil, finalPst)
where
	xCord = (hitPoint.x / TILE_SIZE)					/// pixel to tile coords system
	yCord = (hitPoint.y / TILE_SIZE)					/// pixel to tile coords system
	deHighlight = showValidMoves pst					/// This is probably not the best way to do it but it looks cool so far
	piece = gs.worldMatrix.[xCord  + yCord * 8]			/// getting piece at that index
	newGS = {gs & selectedPiece = piece}				/// new game-state
	newPST = {deHighlight & ls=newGS}					/// updating process state with new GameState
	finalPst = showValidMoves newPST					/// The last pst to be processed 
	
	/*
	* for testing.. comment it once you are done testing
	*
	printingPiece :: (Maybe Piece) -> String
	printingPiece Nothing = "Nothing"
	printingPiece (Just p)
	| p.type == Pawn && p.player == WhitePiece = "whitePawn"
	| p.type == Pawn && p.player == BlackPiece = "blackPawn"
	| p.type == Bishop && p.player == WhitePiece = "whitebishop"
	| p.type == Bishop && p.player == BlackPiece = "blackbishop"
	= toString p.xCord +++ " " +++ toString p.yCord
	*/
mouseHandler _ pst =  pst





m_filter :: MouseState -> Bool
m_filter (MouseMove _ _ ) = False
m_filter _ = True

///____________ Other Window Handling events functions_____________
quit:: (.ls, *PSt .l) -> (.ls, *PSt .l)
quit (local, pst) = (local, closeProcess pst)
		
	
	