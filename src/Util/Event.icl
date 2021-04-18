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
	piece = gs.worldMatrix.[xCord  + yCord * 8]			/// getting piece at that index
	newGS = {gs & selectedPiece = piece}				/// new game-state
	newPST = {pst & ls=newGS}						/// updating process state with new GameState
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
# msg = ("clicked tile: (" +++ toString (hitPoint.x / TILE_SIZE) +++ ", " +++ toString (xC / TILE_SIZE) +++ ")" +++ " The Piece is: " ) 
= (trace_n msg (nil, finalPst))
where
	(xC,yC) = case gs.selectedPiece of
			Nothing = (0,0)
			Just p = (p.xCord,p.yCord)
	xCord = (hitPoint.x / TILE_SIZE)					/// pixel to tile coords system
	yCord = (hitPoint.y / TILE_SIZE)					/// pixel to tile coords system
	deHighlight = showValidMoves pst
	piece = gs.selectedPiece							/// getting piece at that index
	drawPiece = MovePieceFunc xCord yCord piece deHighlight
	drawnPst = case gs.selectedPiece of
			Nothing = drawPiece
			Just p = fillFunc p.xCord p.yCord drawPiece
	newGST = case gs.selectedPiece of
			Nothing = gs
			Just p = updateGST (xCord, yCord) (xC + yC * 8) piece gs
	finalPst = {drawnPst & ls=newGST}


mouseHandler _ pst =  pst	


MovePieceFunc :: Int Int !(Maybe Piece) (*PSt GameState) -> (*PSt GameState)
MovePieceFunc xC yC p pst=:{ls, io} = {pst & io = appWindowPicture (ls.windowId) (renderPieceAt xC yC p) io}

fillFunc :: Int Int (*PSt GameState) -> (*PSt GameState)
fillFunc xC yC pst=:{ls, io} = {pst & io = appWindowPicture (ls.windowId) (fillBoardAt xC yC) io}

updateGST :: (Int,Int) Int !(Maybe Piece) GameState -> GameState
updateGST (move,to) org m gs = switched
where
	s = updatePiece move to m 
	moveTo = move + to * 8
	movedPiece = {gs & worldMatrix = {(\x |x == moveTo = s = a) i \\ a<-:gs.worldMatrix & i<-[0..] }}
	switched = {movedPiece & worldMatrix = {(\x |x == org = Nothing = a) i \\ a<-:movedPiece.worldMatrix & i<-[0..] }}
	
updatePiece :: Int Int !(Maybe Piece) -> !(Maybe Piece)
updatePiece xC yC Nothing = Nothing
updatePiece xC yC (Just p) = Just {p & xCord = xC , yCord = yC}



m_filter :: MouseState -> Bool
m_filter (MouseMove _ _ ) = False
m_filter _ = True

///____________ Other Window Handling events functions_____________
quit:: (.ls, *PSt .l) -> (.ls, *PSt .l)
quit (local, pst) = (local, closeProcess pst)
		
	
	