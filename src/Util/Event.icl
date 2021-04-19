implementation module Util.Event

import StdEnv, StdIO, StdDebug, Util.Constants, Util.Rendering, Util.CostumFunctions


///____________ Mouse Handling events functions_____________


/* 
* testing:- 
mouseHandler (MouseDown hitPoint _ _) (nil, pst=:{ls=gs}) = = trace_n ( toString xCord +++ " " +++ toString yCord  +++ " ." +++ printingPiece gs.selectedPiece) (nil, newPST)
*/

mouseHandler :: MouseState (.ls, *PSt GameState) -> (.ls,*PSt GameState)
mouseHandler (MouseDown hitPoint _ _) (nil, pst=:{ls=gs, io}) 
| hitPoint.x > TILE_SIZE*TILE_SIZE || hitPoint.y > TILE_SIZE*TILE_SIZE = (nil, pst)  //// if the mouseDown event happens out of the picture Context
= (nil, finalPst)
where
	xCord = (hitPoint.x / TILE_SIZE)					/// pixel to tile coords system
	yCord = (hitPoint.y / TILE_SIZE)					/// pixel to tile coords system
	piece = gs.worldMatrix.[xCord  + yCord * 8]			/// getting piece at that index
	newGS = {gs & selectedPiece = piece}				/// new game-state
	newPST = {pst & ls=newGS}						    /// updating process state with new GameState
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
	
mouseHandler (MouseUp hitPoint _) (nil, pst=:{ls=gs, io}) 
| hitPoint.x > TILE_SIZE*TILE_SIZE || hitPoint.y > TILE_SIZE*TILE_SIZE = (nil, deHighlight) // if the mouseUp event happens out of the picture Context
| gs.validMoves.[mouseUpxCord + mouseUpyCord * 8] =  (nil, movePiece mouseUpxCord mouseUpyCord deHighlight ) //if a move is valid, update
= (nil, deHighlight)
where
	mouseUpxCord = (hitPoint.x / TILE_SIZE)					/// pixel to tile coords system
	mouseUpyCord = (hitPoint.y / TILE_SIZE)					/// pixel to tile coords system
	deHighlight  = showValidMoves pst						/// Dehighlight when the mouse goes up
	
mouseHandler _ pst =  pst

///*This function is just to avoid the uniqueness error that occurs when using the pst for more than one thing at a time*/
movePiece :: Int Int (*PSt GameState) -> (*PSt GameState)
movePiece mouseUpxCord mouseUpyCord pst=:{ls=gs, io} = finalPst
where
	finalPst 	 = (\x | x = (UpdateGST mouseUpxCord mouseUpyCord pst) |otherwise = pst) (isJust gs.selectedPiece)


m_filter :: MouseState -> Bool
m_filter (MouseMove _ _ ) = False
m_filter _ = True

///____________ Other Window Handling events functions_____________
quit:: (.ls, *PSt .l) -> (.ls, *PSt .l)
quit (local, pst) = (local, closeProcess pst)
		
	
	