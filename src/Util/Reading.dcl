definition module Util.Reading
import StdIO, StdEnv

::PiecePicture = {
			 tileWidth::Int  , 
			 tileHeight:: Int , 
			 arrayOfPixels :: [RGBColour] 
		   }
instance == RGBColour

getPixels :: *File -> ([RGBColour], *File)

readPicture :: *File -> (PiecePicture,*File)

LoadPicture :: String *World ->  (PiecePicture,*World)
