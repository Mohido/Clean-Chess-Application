implementation module Util.Dialogs

import Util.Reading, Util.Event, Util.Constants, Util.CostumFunctions, Util.Rendering


promotion :: Int (*PSt GameState) -> (*PSt GameState)
promotion mouseUpyCord pSt =:{ls = gs, io}
	|not(/*( gs.selectedPiece.type == Pawn && mouseUpyCord == 7 && gs.selectedPiece.player == BlackPiece ) || 
		( gs.selectedPiece.type == Pawn && mouseUpyCord == 0 && gs.selectedPiece.player == WhitePiece )*/
		case gs.selectedPiece of
					Nothing = False
					Just p = ( ( (p.type == Pawn) && (mouseUpyCord == 7) && (p.player == BlackPiece) ) || 
							   ( (p.type == Pawn) && (mouseUpyCord == 0) && (p.player == WhitePiece)) )
		)	= pSt 		
	# (error ,pst2 ) = open_dialog_promo pSt 
	= pst2
	where
		open_dialog_promo :: (*PSt GameState) -> (ErrorReport, *PSt GameState)
		open_dialog_promo pSt=:{ls,io}
			= (openDialog undef (dialog ls) pSt )
		dialog ls = Dialog "Promotion"
					(CustomButtonControl {w=64,h=64} (\ _ _ =  renderPieceAt 0 0  ls.selectedPiece) [ControlFunction quit]
					//CompoundControl (TextControl "" []) [ControlViewSize {w=256, h=256},ControlLook True (\ _ _ =  renderPieceAt 0 0  ls.selectedPiece),ControlViewDomain pictDomain, ControlFunction closeProcess]  
					/*TextControl "Choose one to do Promotion!" []
					:+: ButtonControl "Queen" [ ControlPos (Left,zero) 
												/* , ControlFunction queenFunc 	*/									      
											  ]
					:+: ButtonControl "Bishop"[ ControlPos (Left,zero) 
											   
										      ]
					:+: ButtonControl "Knight"[ ControlPos (Left,zero) 
												
										   	  ]
					:+: ButtonControl "Rook"  [ ControlPos (Left,zero) 
												
										   	  ]*/
					) [WindowViewSize {w=6*TILE_SIZE,h=128}]
/*
					
queenFunc :: (.ls , *PSt GameState)	 -> (.ls , *PSt GameState)	
queenFunc (ls, pst=:{gs , io}) = (ls , {{gs & gs.selectedPiece 
												= {selectedPiece &
												selectedPiece.type = Queen , 
												selectedPiece.sprite = selectSprite selectedPiece }
										 , io}
where
	(wq_sprite, world2) = LoadPicture "../res/Queen_White.cimg" world1
	(bq_sprite, world3) = LoadPicture "../res/Queen_Black.cimg" world2
	selectSprite :: (Maybe Piece) -> PiecePicture 
	selectSprite p 
	| p.player == BlackPiece = bq_sprite 
	= wq_sprite
*/
/*			
bishopFunc :: (.ls , *PSt GameState)	 -> (.ls , *PSt GameState)	
bishopFunc (ls, pst=:{gs, io}) =  (ls , {{gs & gs.selectedPiece = {selectedPiece 
											& selectedPiece.type = Bishop 
											& selectedPiece.sprite = selectSprite selectedPiece 
									    } , io}
where
	(bishop_w_sprite, world6) = LoadPicture "../res/Bishop_White.cimg" world5		//bishop_white
	(bishop_b_sprite, world7) = LoadPicture "../res/Bishop_Black.cimg" world6		//bishop_black
	selectSprite :: Piece -> PiecePicture 
	selectSprite p 
	| p.player == BlackPiece = bishop_b_sprite 
	= bishop_w_sprite
	
knightFunc :: (.ls , *PSt GameState)	 -> (.ls , *PSt GameState)	
knightFunc (ls, pst=:{gs, io}) = (ls , {{gs & gs.selectedPiece = queenPiece} , io}

rookFunc :: (.ls , *PSt GameState)	 -> (.ls , *PSt GameState)	
rookFunc (ls, pst=:{gs, io}) = (ls , {{gs & gs.selectedPiece = queenPiece} , io}
*/

/*
quit :: (.ls,PSt .GameState) -> (.ls,PSt .GameState)
quit (ls,pSt) = (ls,closeProcess pSt)
*/
