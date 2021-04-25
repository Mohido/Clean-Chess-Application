implementation module Util.GameoverDialog

import StdEnv, StdIO, Util.Constants

GameoverDialog :: (*PSt GameState) -> (*PSt GameState)
GameoverDialog pst=:{ls, io} 
# (okId,pst) = openId pst
# newio 	 = disableWindowMouse ls.windowId pst.io 
= snd (openDialog undef (dialog ls okId) {pst& io = newio})
where 
	dialog ls okId = Dialog (whoWins ls.turnCount)
					( 	  ButtonControl "Exit" [ControlFunction (noLS closeProcess), ControlPos (Center, OffsetAlign AlignCenter)]

					) [WindowViewSize {w=(5*TILE_SIZE - TILE_SIZE/2 ),h=80}, WindowId okId, WindowPos (RightTo ls.windowId, OffsetAlign AlignCenter)]
whoWins :: Int -> String
whoWins 0 = "*******************_Black Wins!_*******************"
whoWins 1 = "*******************_White Wins!_*******************"