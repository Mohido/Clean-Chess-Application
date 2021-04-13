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
	
	/*________ Sprites Loading Area_______________*/
	(wq_sprite, world2) = LoadPicture "white_queen.cimg" world1						//white_queen
	
	(bishop_w_sprite, world3) = LoadPicture "../res/Bishop_White.cimg" world2		//bishop_white
	(bishop_b_sprite, world4) = LoadPicture "../res/Bishop_Black.cimg" world3		//bishop_black
	
	(pawn_w_sprite, world5) = LoadPicture "../res/Pawn_White.cimg" world4 			//pawn_white
	(pawn_b_sprite, world6) = LoadPicture "../res/Pawn_Black.cimg" world5			//pawn_black
	
	(knight_w_sprite, world7) = LoadPicture "../res/Knight_White.cimg" world6 		//knight_white
	(knight_b_sprite, world8) = LoadPicture "../res/Knight_Black.cimg" world7		//knight_black
	
	(rook_w_sprite, world9) = LoadPicture "../res/Rook_White.cimg" world8 			//rook_white
	(rook_b_sprite, worldFinal) = LoadPicture "../res/Rook_Black.cimg" world9		//rook_black
	
	/*_____________Loading pieces Area________________*/ 
	wq_piece = { xCord = 4, yCord = 7,  player = WhitePiece, type = Queen, sprite = wq_sprite} 
			/*left piece of the bishop*/
	bishop_w_piece = { xCord = 2, yCord = 7,  player = WhitePiece, type = Bishop, sprite = bishop_w_sprite} 
	bishop_b_piece = { xCord = 2, yCord = 0,  player = BlackPiece, type = Bishop, sprite = bishop_b_sprite} 
			/*left piece of the pawn*/
	pawn_w_piece = { xCord = 0, yCord = 6,  player = WhitePiece, type = Pawn, sprite = pawn_w_sprite} 
	pawn_b_piece = { xCord = 0, yCord = 1,  player = BlackPiece, type = Pawn, sprite = pawn_b_sprite} 
			/*Left piece of the knight*/
	knight_w_piece = { xCord = 1, yCord = 7,  player = WhitePiece, type = Knight, sprite = knight_w_sprite} 
	knight_b_piece = { xCord = 1, yCord = 0,  player = BlackPiece, type = Knight, sprite = knight_b_sprite} 
			/*Left piece of the Rook*/
	rook_w_piece = { xCord = 0, yCord = 7,  player = WhitePiece, type = Rook, sprite = rook_w_sprite} 
	rook_b_piece = { xCord = 0, yCord = 0,  player = BlackPiece, type = Rook, sprite = rook_b_sprite} 
	
	/*Initial board creation*/
	initBoard = [Just rook_b_piece, Just knight_b_piece, Just bishop_b_piece, Nothing, Nothing, Just {bishop_b_piece & xCord = 5}, Just {knight_b_piece & xCord = 6}, Just {rook_b_piece & xCord = 7}] ++  	// black Main pieces area 
				[Just { pawn_b_piece & xCord = x }  \\ x <- [0..7]] ++	// black pawns
				[Nothing \\ s <- [0..7], y <- [1..4]] ++ 				 //middle empty area.
				[Just { pawn_w_piece & xCord = x}  \\ x <- [0..7]] ++	// white pawns
				[Just rook_w_piece, Just knight_w_piece, Just bishop_w_piece, Nothing, Just wq_piece, Just {bishop_w_piece & xCord = 5}, Just {knight_w_piece & xCord = 6}, Just {rook_w_piece & xCord = 7}] 	// white Main pieces area
				
	// main board, with 8*8 tile of pieces or nothing.
	board :: !Board
	board =  { p \\ p <- initBoard } 	//Main Initial board.

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
									WindowLook False (look preBoard) ,					/// This will take the state and update state away.
									WindowMouse m_filter Able mouseHandler				/// Funcitons in Even.dcl. It handles mouse events
								]