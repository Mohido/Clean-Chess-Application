module Chess_Board

import StdEnv, StdIO, StdFunc, StdDebug ///StdFunc contains seq, StdDebug contains trace_n
import Util.Reading, Util.Event, Util.Constants, Util.CostumFunctions, Util.Rendering



//Start Function and Initializing game assets
Start:: *World -> *World
Start world 
# gs = { turnCount=0,players=initPlayers , worldMatrix = board,
		 selectedPiece = Nothing, windowId = wid, validMoves = initMoves , 
		 sprites = {whiteKing = wking_sprite , blackKing = bking_sprite, 
		 			whiteQueen = wq_sprite , blackQueen = bq_sprite,
		 			whiteBishop = bishop_w_sprite , blackBishop = bishop_b_sprite,
		 			whiteRook = rook_w_sprite , blackRook = rook_b_sprite,
		 			whiteKnight = knight_w_sprite, blackKnight = knight_b_sprite,
		 			whitePawn = pawn_w_sprite, blackPawn = pawn_b_sprite   }}  		///initial game state (process state)
= startIO SDI gs (initIO (wid, board) ) [ProcessClose closeProcess] worldFinal
where
	/*_______ world function + reading sprites________*/
	(wid ,world1 ) = openId world
	
	/*________ Sprites Loading Area_______________*/
	(wq_sprite, world2) = LoadPicture "../res/Queen_White.cimg" world1			//white_queen
	(bq_sprite, world3) = LoadPicture "../res/Queen_Black.cimg" world2              	//black_queen
	
	(wking_sprite, world4) = LoadPicture "../res/King_White.cimg" world3			//white_king
	(bking_sprite, world5) = LoadPicture "../res/King_Black.cimg" world4			//black_king
	
	(bishop_w_sprite, world6) = LoadPicture "../res/Bishop_White.cimg" world5		//bishop_white
	(bishop_b_sprite, world7) = LoadPicture "../res/Bishop_Black.cimg" world6		//bishop_black
	
	(pawn_w_sprite, world8) = LoadPicture "../res/Pawn_White.cimg" world7 			//pawn_white
	(pawn_b_sprite, world9) = LoadPicture "../res/Pawn_Black.cimg" world8			//pawn_black
	
	(knight_w_sprite, world10) = LoadPicture "../res/Knight_White.cimg" world9 		//knight_white
	(knight_b_sprite, world11) = LoadPicture "../res/Knight_Black.cimg" world10		//knight_black
	
	(rook_w_sprite, world12) = LoadPicture "../res/Rook_White.cimg" world11 		//rook_white
	(rook_b_sprite, worldFinal) = LoadPicture "../res/Rook_Black.cimg" world12		//rook_black
	
	
	/*_____________Loading pieces Area________________*/ 
	       		 /*queen*/
	wq_piece = { xCord = 3, yCord = 7,  player = WhitePiece, type = Queen, sprite = wq_sprite} 
	bq_piece = { xCord = 3, yCord = 0,  player = BlackPiece, type = Queen, sprite = bq_sprite} 
	        	 /*king*/
	wking_piece = { xCord = 4, yCord = 7, player = WhitePiece, type = King, sprite = wking_sprite}
	bking_piece = { xCord = 4, yCord = 0, player = BlackPiece, type = King, sprite = bking_sprite}
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
	initBoard = [Just rook_b_piece, Just knight_b_piece, Just bishop_b_piece, Just bq_piece, Just bking_piece,  Just {bishop_b_piece & xCord = 5}, Just {knight_b_piece & xCord = 6}, Just {rook_b_piece & xCord = 7}] ++  	// black Main pieces area 
				[Just { pawn_b_piece & xCord = x }  \\ x <- [0..7]] ++	// black pawns
				[Nothing \\ s <- [0..7], y <- [1..4]] ++ 				 //middle empty area.
				[Just { pawn_w_piece & xCord = x}  \\ x <- [0..7]] ++	// white pawns
				[Just rook_w_piece, Just knight_w_piece, Just bishop_w_piece,  Just wq_piece, Just wking_piece, Just {bishop_w_piece & xCord = 5}, Just {knight_w_piece & xCord = 6}, Just {rook_w_piece & xCord = 7}] 	// white Main pieces area
				
	// main board, with 8*8 tile of pieces or nothing.
	board :: !Board
	board =  { p \\ p <- initBoard } 	//Main Initial board.

//_____________________________________________________________________________



//____________ Initializing the window area

initIO :: (!Id, {!(Maybe Piece)}) (PSt GameState) -> (PSt GameState)
initIO w_inits=:(wid, preBoard) pst=:{ls}
# (errorM, newPst) = openWindow undef (window) pst
//# finalPst = {newPst & io = setWindowLook wid True (True,look (False,ls.worldMatrix)) newPst.io } 
= newPst
where
	window = Window "Title" NilLS
								[	
									WindowId wid,
									WindowClose quit, 
									WindowActivate updateWindowLook, /// Changing the look of the window once it is activated...
									WindowViewSize {w = 8*TILE_SIZE, h = 8*TILE_SIZE}, 	/// defining the size of the window.
									WindowMouse m_filter Able mouseHandler,			/// Funcitons in Even.dcl. It handles mouse events
									WindowInit updateWindowLook
								]
	where 
		updateWindowLook :: (.ls, *PSt GameState) -> (.ls, *PSt GameState)
		updateWindowLook (nil, upst) 
		# (nil, last)  = fullWindowLook (nil, upst) 
		= (nil, {last & io = setWindowLook last.ls.windowId False (False, look (False,last.ls.worldMatrix)) last.io } )
		
		fullWindowLook :: (.ls, *PSt GameState) -> (.ls, *PSt GameState)
		fullWindowLook (nil, upst) = (nil, {upst & io = setWindowLook upst.ls.windowId True (False, look (True,upst.ls.worldMatrix)) upst.io } )
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		