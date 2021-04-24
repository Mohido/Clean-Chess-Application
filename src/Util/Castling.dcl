definition module Util.Castling
import StdEnv, StdIO, Util.Constants, Util.Rendering


disableRightCastle :: (*PSt GameState) -> (*PSt GameState) 
	
disableLeftCastle :: (*PSt GameState) -> (*PSt GameState) 
	
disableBothCastle :: (*PSt GameState) -> (*PSt GameState) 

checkCastle	:: Int Int (*PSt GameState) -> (*PSt GameState) 
