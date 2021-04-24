definition module Util.Reading
import StdIO, StdEnv, Util.Constants

getPixels :: *File -> ([RGBColour], *File)

readPicture :: *File -> (PiecePicture,*File)

getArrayOfPixels :: ([RGBColour], *File) -> ({#RGBColour}, *File)

LoadPicture :: String *World ->  (PiecePicture,*World)
