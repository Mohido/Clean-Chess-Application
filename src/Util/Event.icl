implementation module Util.Event

import StdEnv, StdIO, StdDebug, Util.Constants, Util.Rendering, Util.CostumFunctions, Util.Dialogs, Util.Sounds, Util.Castling, Util.GameoverDialog
import Util.GameOverFunctions


///____________ Mouse Handling events functions_____________

mouseHandler :: MouseState (.ls, *PSt GameState) -> (.ls,*PSt GameState)
mouseHandler (MouseDown hitPoint _ _) (nil, pst=:{ls=gs, io}) 
| hitPoint.x > 512 || hitPoint.y > 512 = (nil, pst) 	    /// if the mouseDown event happens out of the picture Context
| (isJust piece) && ( currentColor == (fromJust piece).player) = (nil, finalPst) 
= (nil, pst)
where
	xCord 		 = (hitPoint.x / TILE_SIZE)					/// pixel to tile coords system
	yCord 		 = (hitPoint.y / TILE_SIZE)					/// pixel to tile coords system
	piece 		 = gs.worldMatrix.[xCord  + yCord * 8]		/// getting piece at that index
	currentColor = gs.players.[gs.turnCount].colour			/// Current turn colour
	newGS 		 = {gs & selectedPiece = piece}				/// new game-state
	newPST 		 = {pst & ls=newGS}						    /// updating process state with new GameState
	finalPst 	 = showValidMoves True newPST				/// The last pst to be processed 
	
	
/*________Mouse Up______________ */
mouseHandler (MouseUp hitPoint _) (nil, pst=:{ls=gs, io}) 
| hitPoint.x > 512 || hitPoint.y > 512 = (nil, {deHighlight & ls.selectedPiece = Nothing}) // if the mouseUp event happens out of the picture Context
| (isNothing gs.selectedPiece) = (nil,pst)												   // seeing if a piece is selected currently, if not, do nothing 
| not gs.validMoves.[index] =  	(nil, {deHighlight & ls.selectedPiece = Nothing} )		   // if a move is valid, update
| game_over = (nil, GameoverDialog (playSoundCheckMate returnPst))
=  (nil, returnPst)																		   // if nothing then just dehighlight and move on
where
	index 		 = mouseUpxCord + mouseUpyCord * 8
	mouseUpxCord = (hitPoint.x / TILE_SIZE)													/// pixel to tile coords system
	mouseUpyCord = (hitPoint.y / TILE_SIZE)													/// pixel to tile coords system
	deHighlight  = showValidMoves True pst 													/// Dehighlight when the mouse goes up
	editedPst    = UpdateGST mouseUpxCord mouseUpyCord deHighlight							/// move the piece and update the gameState 
	changedTurns = {editedPst & ls.turnCount = (editedPst.ls.turnCount + 1) rem 2}			/// take the edited state and update the turns
	(g_state, tempPst) = getGameState changedTurns											/// a valid move accured here! .. now check if game is over!
	shiftedPlayer = (g_state.players.[g_state.turnCount] ).colour
	isChecked = isUnderCheck shiftedPlayer g_state.worldMatrix
	tempPst2 = case isChecked of 
					True = playSoundCheck (disableBothCastle tempPst)
					False = tempPst
	(game_over, returnPst) = isGameOver shiftedPlayer tempPst2 							    //isUnderCheck BlackPiece game_s.worldMatrix
mouseHandler _ pst =  pst
	 
	
/*________Other Mouse Events______________ */

m_filter :: MouseState -> Bool
m_filter (MouseMove _ _ ) = False
m_filter _ = True

///____________ Other Window Handling events functions_____________
quit:: (.ls, *PSt .l) -> (.ls, *PSt .l)
quit (local, pst) = (local, closeProcess pst)
		
		
		
		
		
		
		
		
		
		
		
		
		
	
	