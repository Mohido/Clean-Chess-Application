implementation module Util.Dialogs

import Util.Reading, Util.Event, Util.Constants, Util.CostumFunctions, Util.Rendering


/*__________________The dialog function to do the promotion__________________*/
promotion :: Int (*PSt GameState) -> (*PSt GameState)
promotion mouseUpyCord pSt =:{ls = gs, io}
	# (error ,pst2 ) = open_dialog_promo_white pSt 
	# (error ,pst3 ) = open_dialog_promo_black pSt 
	# p = fromJust gs.selectedPiece
	| ( case gs.selectedPiece of
					Nothing = False
					Just p = ( (p.type == Pawn) && (mouseUpyCord == 7) && (p.player == BlackPiece)) 
		 )	  = Update p.xCord p.yCord pSt2
	| ( case gs.selectedPiece of
					Nothing = False
					Just p = ( (p.type == Pawn) && (mouseUpyCord == 0) && (p.player == WhitePiece)) 
		 )	  = Update p.xCord p.yCord  pSt3
	= pSt //Update p.xCord p.yCord  pst2
	where
		open_dialog_promo_white :: (*PSt GameState) -> (ErrorReport, *PSt GameState)
		open_dialog_promo_white pSt=:{ls, io} 
			# (okId,pSt) = openId pSt
			= (openDialog undef (dialog ls okId) pSt)
		dialog ls okId = Dialog "Choose one for Promotion"
					( 	  CustomButtonControl {w=64,h=64} (\ _ _ = paintPiece ls.sprites.whiteQueen {x = 0, y = 0} ) 	[ControlFunction whiteQueenFunc]
					  :+: CustomButtonControl {w=64,h=64} (\ _ _ = paintPiece ls.sprites.whiteBishop {x = 0, y = 0} ) 	[ControlFunction whiteBishopFunc]
					  :+: CustomButtonControl {w=64,h=64} (\ _ _ = paintPiece ls.sprites.whiteRook {x = 0, y = 0} ) 	[ControlFunction whiteRookFunc]
					  :+: CustomButtonControl {w=64,h=64} (\ _ _ = paintPiece ls.sprites.whiteKnight {x = 0, y = 0} ) 	[ControlFunction whiteKnightFunc]
					) [WindowViewSize {w=4*TILE_SIZE,h=128}, WindowId okId ]
						 
		open_dialog_promo_black :: (*PSt GameState) -> (ErrorReport, *PSt GameState)
		open_dialog_promo_white pSt=:{ls, io} 
			# (okId,pSt) = openId pSt
			= (openDialog undef (dialog ls okId) pSt)
		dialog ls okId = Dialog "Promotion"
					( 	  CustomButtonControl {w=64,h=64} (\ _ _ = paintPiece ls.sprites.blackQueen {x = 0, y = 0} ) 	[ControlFunction blackQueenFunc]
					  :+: CustomButtonControl {w=64,h=64} (\ _ _ = paintPiece ls.sprites.blackBishop {x = 0, y = 0} ) 	[ControlFunction blackBishopFunc]
					  :+: CustomButtonControl {w=64,h=64} (\ _ _ = paintPiece ls.sprites.blackRook {x = 0, y = 0} ) 	[ControlFunction blackRookFunc]
					  :+: CustomButtonControl {w=64,h=64} (\ _ _ = paintPiece ls.sprites.blackKnight {x = 0, y = 0} ) 	[ControlFunction blackKnightFunc]
					) [WindowViewSize {w=6*TILE_SIZE,h=128}, WindowId okId ]
					


/*__________________The functions for changing the game state__________________*/
whiteQueenFunc:: (.ls, *PSt GameState) -> (.ls, *PSt GameState) 
whiteQueenFunc (nil , pst=:{ls = gs , io}) 
	# p = fromJust gs.selectedPiece
	= ( nil , closeActiveWindow { pst &
				ls = { gs & selectedPiece = Just {p & type  = Queen ,sprite = gs.sprites.whiteQueen , yCord = (p.yCord - 1) }},
			  io = io}
	  )
	  
blackQueenFunc:: (.ls, *PSt GameState) -> (.ls, *PSt GameState) 
blackQueenFunc (nil , pst=:{ls = gs , io}) 
	# p = fromJust gs.selectedPiece
	= ( nil , closeActiveWindow { pst &
				ls = { gs & selectedPiece = Just {p & type  = Queen ,sprite = gs.sprites.blackQueen , yCord = (p.yCord + 1) }},
			  io = io}
	  )	  

whiteBishopFunc:: (.ls, *PSt GameState) -> (.ls, *PSt GameState) 
whiteQueenFunc (nil , pst=:{ls = gs , io}) 
	# p = fromJust gs.selectedPiece
	= ( nil , closeActiveWindow { pst &
				ls = { gs & selectedPiece = Just {p & type  = Bishop ,sprite = gs.sprites.whiteBishop , yCord = (p.yCord - 1) }},
			  io = io}
	  )
	  
blackBishopFunc:: (.ls, *PSt GameState) -> (.ls, *PSt GameState) 
blackQueenFunc (nil , pst=:{ls = gs , io}) 
	# p = fromJust gs.selectedPiece
	= ( nil , closeActiveWindow { pst &
				ls = { gs & selectedPiece = Just {p & type  = Bishop ,sprite = gs.sprites.blackBishop , yCord = (p.yCord + 1) }},
			  io = io}
	  )

whiteRookFunc:: (.ls, *PSt GameState) -> (.ls, *PSt GameState) 
whiteQueenFunc (nil , pst=:{ls = gs , io}) 
	# p = fromJust gs.selectedPiece
	= ( nil , closeActiveWindow { pst &
				ls = { gs & selectedPiece = Just {p & type  = Rook ,sprite = gs.sprites.whiteRook , yCord = (p.yCord - 1) }},
			  io = io}
	  )
	  
blackRookFunc:: (.ls, *PSt GameState) -> (.ls, *PSt GameState) 
blackQueenFunc (nil , pst=:{ls = gs , io}) 
	# p = fromJust gs.selectedPiece
	= ( nil , closeActiveWindow { pst &
				ls = { gs & selectedPiece = Just {p & type  = Rook ,sprite = gs.sprites.blackRook , yCord = (p.yCord + 1) }},
			  io = io}
	  )	  

whiteKnightFunc:: (.ls, *PSt GameState) -> (.ls, *PSt GameState) 
whiteQueenFunc (nil , pst=:{ls = gs , io}) 
	# p = fromJust gs.selectedPiece
	= ( nil , closeActiveWindow { pst &
				ls = { gs & selectedPiece = Just {p & type  = Knight ,sprite = gs.sprites.whiteKnight , yCord = (p.yCord - 1) }},
			  io = io}
	  )
	  
blackKnightFunc:: (.ls, *PSt GameState) -> (.ls, *PSt GameState) 
blackQueenFunc (nil , pst=:{ls = gs , io}) 
	# p = fromJust gs.selectedPiece
	= ( nil , closeActiveWindow { pst &
				ls = { gs & selectedPiece = Just {p & type  = Knight ,sprite = gs.sprites.blackKnight , yCord = (p.yCord + 1) }},
			  io = io}
	  )	  

/*__________________The drawing function__________________*/
paintPiece :: PiecePicture !Point2  *Picture -> *Picture
paintPiece pie coord pic  = paintPieceAux  (pointsAndColours) pic 
where 
	pointsAndColours = [ ((coord.x*TILE_SIZE + yPixel, coord.y*TILE_SIZE + xPixel)
		, getPixelValue (xPixel, yPixel) pie )
		\\ yPixel <- [0..TILE_SIZE] ,  xPixel <- [0..TILE_SIZE] | not ( getPixelValue (xPixel, yPixel) pie == {r=255, g=0, b=255}) ]
	paintPieceAux :: [((Int ,Int) , RGBColour )] *Picture -> *Picture
	paintPieceAux [] pic = pic
	paintPieceAux [( (x,y) , rgb ) : rest ] pic
		# pic = setPenColour (RGB rgb) pic 
	= paintPieceAux rest (drawPointAt {x = x, y = y} pic)

/*__________________The updating function for the game state__________________*/
Update :: Int Int (*PSt GameState) -> (*PSt GameState)
Update mouseUpxCord mouseUpyCord pst=:{ls=gs, io}
| mouseUpxCord == selectedxCord && selectedyCord == mouseUpyCord = pst
=lastPst  
where
	piece		 = gs.selectedPiece
	(selectedxCord,selectedyCord) = case piece of 
										Nothing = (0,0)
										Just p = (p.xCord,p.yCord)
	pieceRendered = MovePiece     (selectedxCord,selectedyCord)  mouseUpxCord mouseUpyCord pst
	updatedIo     = setWindowLook (pieceRendered.ls.windowId) False (False, look pieceRendered.ls.worldMatrix) pieceRendered.io
	lastPst       = {pieceRendered & ls.selectedPiece = Nothing,io = updatedIo}

