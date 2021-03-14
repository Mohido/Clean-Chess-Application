module FirstSteps

import StdEnv


CharListWrite :: [Char] *File -> *File
CharListWrite [] f = f
CharListWrite [c:cs] f = CharListWrite cs (fwritec c f)

CharFileCopy :: File *File -> *File
CharFileCopy infile outfile = CharListWrite (CharListRead infile) outfile

CharListRead :: File -> [Char]
CharListRead f
| not readok = []
|otherwise = [char : CharListRead filewithchangedreadpointer]
where
	(readok,char,filewithchangedreadpointer) = sfreadc f


CopyFile :: String String *env -> *env | FileSystem env
CopyFile inputfname outputfname filesys
| readok && writeok && closeok
= finalfilesystem
| not readok = abort ("Cannot open input file: '" +++ inputfname +++ "'")
| not writeok = abort ("Cannot open output file: '" +++ outputfname +++ "'")
| not closeok = abort ("Cannot close output file: '" +++ outputfname +++ "'")
where
	(readok,inputfile,touchedfilesys) = sfopen inputfname FReadText filesys
	(writeok,outputfile,nwfilesys) = fopen outputfname FWriteText touchedfilesys
	copiedfile = CharFileCopy inputfile outputfile
	(closeok,finalfilesystem) = fclose copiedfile nwfilesys

inputfilename :== "in.txt"
outputfilename :== "out.txt"

//Start :: *World -> *World
//Start world = CopyFile "in.txt" "out.txt" world
/*
Start :: *World -> *World
Start world
# (console,world) = stdio world
# (atring,console) = freadline console
# console = fwrites atring console
# (ok,world) = fclose console world
| not ok = abort "Cannot close console.\n"
| otherwise = world
*/