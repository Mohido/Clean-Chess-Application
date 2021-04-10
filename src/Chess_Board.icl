module Chess_Board

import StdEnv, StdIO, StdFunc, StdDebug ///StdFunc contains seq, StdDebug contains trace_n
import Util.Reading, Util.Event, Util.Constants, Util.CostumFunctions, Util.Rendering


//Start Function and Initializing game assets
Start:: *World -> *World
Start world 
# gs = {worldMatrix = board, selectedPiece = Nothing, windowId = wid}  		///initial game state (process state)
= startIO SDI gs (initIO (wid, board) ) [ProcessClose closeProcess] worldFinal
where
	/*_______ world function + reading sprites________*/
	(wid ,world1 ) = openId world
	(wq_sprite, worldFinal) = LoadPicture "white_queen.cimg" world1
	
	
	/*_____________Loading pieces Area________________*/ 
	wq_piece = { xCord = 4, yCord = 7,  player = WhitePiece, type = Queen, sprite = wq_sprite} 
	
	// main board, with 8*8 pieces or nothing.
	board :: !Board
	board = {Just wq_piece}

//_____________________________________________________________________________







//____________ Initializing the window area

initIO :: (!Id, {!(Maybe Piece)}) (PSt GameState) -> (PSt GameState)
initIO w_inits=:(wid, preBoard) pst
# (errorM, pst) = openWindow undef (window) pst
= pst
where
	window = Window "Title" NilLS
								[	
									WindowId wid,
									WindowClose quit, 
									WindowViewSize {w = 8*TILE_SIZE, h = 8*TILE_SIZE}, 	/// defining the size of the window.
									WindowLook False (look preBoard) 				/// This will take the state and update state away.
								]



	
	
	
	
	
	
	
	
	
	