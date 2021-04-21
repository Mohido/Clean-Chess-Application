definition module Util.Sounds
import StdEnv, StdIO, Util.Constants


playSoundmove :: (*PSt GameState) -> (*PSt GameState) 


playSoundCapture :: (*PSt GameState) -> (*PSt GameState) 

class playSoundFile env :: !String !*env -> (!Bool,!*env)

/*	playSoundFile filename 
		opens the sound file at filename and plays it synchronously. 
		The Boolean result indicates whether the sound file could be succesfully 
		played.
*/

instance playSoundFile World
