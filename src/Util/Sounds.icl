implementation module Util.Sounds

import iostate ,StdEnv, StdIO, Util.Constants
import clCCall_12, ostoolbox

playSoundmove :: (*PSt GameState) -> (*PSt GameState) 
playSoundmove pst=:{ls,io}
# (world, io)  = ioStGetWorld io 
# (bol, world) = playSoundFile "./Util/sounds/move.WAV" world
# io2 = ioStSetWorld world io 
= {pst & io = io2}

playSoundpromotion :: (*PSt GameState) -> (*PSt GameState) 
playSoundpromotion pst=:{ls,io}
# (world, io)  = ioStGetWorld io 
# (bol, world) = playSoundFile "./Util/sounds/promotion.WAV" world
# io2 = ioStSetWorld world io 
= {pst & io = io2}

playSoundCapture :: (*PSt GameState) -> (*PSt GameState) 
playSoundCapture pst=:{ls,io}
# (world, io)  = ioStGetWorld io 
# (bol, world) = playSoundFile "./Util/sounds/capture.WAV" world
# io2 = ioStSetWorld world io 
= {pst & io = io2}

playSoundCastling :: (*PSt GameState) -> (*PSt GameState) 
playSoundCastling pst=:{ls,io}
# (world, io)  = ioStGetWorld io 
# (bol, world) = playSoundFile "./Util/sounds/castling.WAV" world
# io2 = ioStSetWorld world io 
= {pst & io = io2}

playSoundCheck :: (*PSt GameState) -> (*PSt GameState) 
playSoundCheck pst=:{ls,io}
# (world, io)  = ioStGetWorld io 
# (bol, world) = playSoundFile "./Util/sounds/check.WAV" world
# io2 = ioStSetWorld world io 
= {pst & io = io2}

playSoundCheckMate :: (*PSt GameState) -> (*PSt GameState) 
playSoundCheckMate pst=:{ls,io}
# (world, io)  = ioStGetWorld io 
# (bol, world) = playSoundFile "./Util/sounds/checkmate.WAV" world
# io2 = ioStSetWorld world io 
= {pst & io = io2}



class playSoundFile env :: !String !*env -> (!Bool,!*env)

instance playSoundFile World where
	playSoundFile :: !String !*World -> (!Bool,!*World)
	playSoundFile soundFileName world
		# (tb,world)	= worldGetToolbox world
		# (ok,tb)		= winPlaySound soundFileName tb
	    # world			= worldSetToolbox tb world
	    = (ok,world)
