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
	(wq_sprite, world2) = LoadPicture "white_queen.cimg" world1					//white_queen
	

	
	(bp_w_sprite, world3) = LoadPicture "../res/Bishop_White.cimg" world2		//bishop_white
	(bp_b_sprite, world4) = LoadPicture "../res/Bishop_Black.cimg" world3		//bishop_black
	
	(p_w_sprite, world5) = LoadPicture "../res/Pawn_White.cimg" world4 			//pawn_white
	(p_b_sprite, worldFinal) = LoadPicture "../res/Pawn_Black.cimg" world5		//pawn_black
	
	/*_____________Loading pieces Area________________*/ 
	wq_piece = { xCord = 4, yCord = 7,  player = WhitePiece, type = Queen, sprite = wq_sprite} 
	
			/*left piece of the bishop*/
	bp_w_piece = { xCord = 2, yCord = 7,  player = WhitePiece, type = Bishop, sprite = bp_w_sprite} 
	bp_b_piece = { xCord = 2, yCord = 0,  player = BlackPiece, type = Bishop, sprite = bp_b_sprite} 
			/*left piece of the pawn*/
	p_w_piece = { xCord = 0, yCord = 6,  player = WhitePiece, type = Pawn, sprite = p_w_sprite} 
	p_b_piece = { xCord = 0, yCord = 1,  player = BlackPiece, type = Pawn, sprite = p_b_sprite} 
	
	initBoard = [Nothing, Nothing, Just bp_b_piece, Nothing, Nothing, Just {bp_b_piece & xCord = 5}, Nothing, Nothing] ++ 
				[Just { xCord = x, yCord = 1,  player = BlackPiece, type = Pawn, sprite = p_b_sprite}  \\ x <- [0..7]] ++
				[Nothing \\ s <- [0..7], y <- [1..4]] ++  //middle empty area.
				[Just { xCord = x, yCord = 6,  player = WhitePiece, type = Pawn, sprite = p_w_sprite}  \\ x <- [0..7]] ++
				[Nothing, Nothing, Just bp_w_piece, Nothing, Just wq_piece, Just {bp_w_piece & xCord = 5}, Nothing, Nothing]
				
	// main board, with 8*8 pieces or nothing.
	board :: !Board
	board =  { p \\ p <- initBoard } 	//Initial board.

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
									WindowLook False (look preBoard) ,				/// This will take the state and update state away.
									WindowMouse m_filter Able mouseHandler
								]




	