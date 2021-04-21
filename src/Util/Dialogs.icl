implementation module Util.Dialogs

import Util.Reading, Util.Event, Util.Constants, Util.CostumFunctions, Util.Rendering


/*__________________The dialog function to do the promotion__________________*/
promotion :: Int Int (*PSt GameState) -> (*PSt GameState)
promotion mouseUpxCord mouseUpyCord pst =:{ls = gs, io}
	# p = fromJust gs.selectedPiece
	| ( (p.type == Pawn) && (mouseUpyCord == 7) && (p.player == BlackPiece)) = snd (open_dialog_promo_black pst) 
	| ( (p.type == Pawn) && (mouseUpyCord == 0) && (p.player == WhitePiece)) = snd (open_dialog_promo_white pst)
	= pst
	where
		open_dialog_promo_white :: (*PSt GameState) -> (ErrorReport, *PSt GameState)
		open_dialog_promo_white pst=:{ls, io} 
			# (okId,pst) = openId pst
			= (openDialog undef (dialog ls okId) pst)
		dialog ls okId = Dialog "Choose one for Promotion"
					( 	  CustomButtonControl {w=64,h=64} (\ _ _ = paintPiece ls.sprites.whiteQueen {x = 0, y = 0} ) 	[ControlFunction (promotePawn mouseUpxCord mouseUpyCord "whiteQueen")]
					  :+: CustomButtonControl {w=64,h=64} (\ _ _ = paintPiece ls.sprites.whiteBishop {x = 0, y = 0} ) 	[ControlFunction (promotePawn mouseUpxCord mouseUpyCord "whiteBishop")]
					  :+: CustomButtonControl {w=64,h=64} (\ _ _ = paintPiece ls.sprites.whiteRook {x = 0, y = 0} ) 	[ControlFunction (promotePawn mouseUpxCord mouseUpyCord "whiteRook")]
					  :+: CustomButtonControl {w=64,h=64} (\ _ _ = paintPiece ls.sprites.whiteKnight {x = 0, y = 0} ) 	[ControlFunction (promotePawn mouseUpxCord mouseUpyCord "whiteKnight")]
					) [WindowViewSize {w=(5*TILE_SIZE - TILE_SIZE/2 ),h=80}, WindowId okId, WindowPos (RightTo ls.windowId, OffsetAlign AlignCenter) ]
						 
		open_dialog_promo_black :: (*PSt GameState) -> (ErrorReport, *PSt GameState)
		open_dialog_promo_black pst=:{ls, io} 
			# (okId,pst) = openId pst
			= (openDialog undef (dialog1 ls okId) pst)
		dialog1 ls okId = Dialog "Choose one for Promotion"
					( 	  CustomButtonControl {w=64,h=64} (\ _ _ = paintPiece ls.sprites.blackQueen {x = 0, y = 0} ) 	[ControlFunction (promotePawn mouseUpxCord mouseUpyCord "blackQueen")]
					  :+: CustomButtonControl {w=64,h=64} (\ _ _ = paintPiece ls.sprites.blackBishop {x = 0, y = 0} ) 	[ControlFunction (promotePawn mouseUpxCord mouseUpyCord "blackBishop")]
					  :+: CustomButtonControl {w=64,h=64} (\ _ _ = paintPiece ls.sprites.blackRook {x = 0, y = 0} ) 	[ControlFunction (promotePawn mouseUpxCord mouseUpyCord "blackRook")]
					  :+: CustomButtonControl {w=64,h=64} (\ _ _ = paintPiece ls.sprites.blackKnight {x = 0, y = 0} ) 	[ControlFunction (promotePawn mouseUpxCord mouseUpyCord "blackKnight")]
					) [WindowViewSize {w=6*TILE_SIZE,h=128}, WindowId okId, WindowPos (RightTo ls.windowId, OffsetAlign AlignCenter) ]


/*_______________Change into___________*/
promotePawn :: Int Int String (.ls, *PSt GameState) -> (.ls, *PSt GameState)
promotePawn  mouseUpxCord mouseUpyCord choice (nil , pst=:{ls, io}) 
# oldPiece = fromJust ls.selectedPiece
# newPiece = case choice of 
			"whiteQueen"	= Just {oldPiece & player=WhitePiece,type=Queen  ,sprite=ls.sprites.whiteQueen}
			"whiteBishop"	= Just {oldPiece & player=WhitePiece,type=Bishop ,sprite=ls.sprites.whiteBishop}
			"whiteRook"		= Just {oldPiece & player=WhitePiece,type=Rook   ,sprite=ls.sprites.whiteRook}
			"whiteKnight"	= Just {oldPiece & player=WhitePiece,type=Knight ,sprite=ls.sprites.whiteKnight}
			"blackQueen"	= Just {oldPiece & player=BlackPiece,type=Queen  ,sprite=ls.sprites.blackQueen}
			"blackBishop"	= Just {oldPiece & player=BlackPiece,type=Bishop ,sprite=ls.sprites.blackBishop}
			"blackRook"		= Just {oldPiece & player=BlackPiece,type=Rook   ,sprite=ls.sprites.blackRook}
			"blackKnight"	= Just {oldPiece & player=BlackPiece,type=Knight ,sprite=ls.sprites.blackKnight}
#newPst    = MovePiece(oldPiece.xCord,oldPiece.yCord) mouseUpxCord mouseUpyCord {pst & ls.selectedPiece = newPiece}
#updatedIo = setWindowLook (newPst.ls.windowId) False (False, look newPst.ls.worldMatrix) newPst.io
=(nil, closeActiveWindow ({newPst & io = updatedIo}))

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