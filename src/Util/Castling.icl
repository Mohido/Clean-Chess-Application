implementation module Util.Castling
import StdEnv, StdIO, Util.Constants, Util.Rendering, Util.Sounds

disableRightCastle :: (*PSt GameState) -> (*PSt GameState) 
disableRightCastle  pst=:{ls=gs,io} = {pst & ls.players = editedPlayers}  
where
	editedPlayers = {(\x | x <> gs.turnCount = a = {a & castleRight = False}) b \\ a<-:gs.players & b<-[0..]}
	
disableLeftCastle :: (*PSt GameState) -> (*PSt GameState) 
disableLeftCastle  pst=:{ls=gs,io} = {pst & ls.players = editedPlayers}  
where
	editedPlayers = {(\x | x <> gs.turnCount = a = {a & castleLeft = False}) b \\ a<-:gs.players & b<-[0..]}
	
disableBothCastle :: (*PSt GameState) -> (*PSt GameState) 
disableBothCastle  pst=:{ls=gs,io} = {pst & ls.players = editedPlayers}  
where
	editedPlayers = {(\x | x <> gs.turnCount = a = {a & castleRight = False, castleLeft = False}) b \\ a<-:gs.players & b<-[0..]}

checkCastle	:: Int Int (*PSt GameState) -> (*PSt GameState) 
checkCastle mouseUpxCord mouseUpyCord pst=:{ls=gs,io}
| mouseUpxCord == 6 && gs.players.[gs.turnCount].castleRight = playSoundCastling castleRight 
| mouseUpxCord == 2 && gs.players.[gs.turnCount].castleLeft  = playSoundCastling castleLeft
= pst
where
	piece = fromJust gs.selectedPiece
	(selectedxCord,selectedyCord) = (piece.xCord,piece.yCord)
	moveRookLeft = MovePiece (7,mouseUpyCord) 5 mouseUpyCord gs.worldMatrix.[7+mouseUpyCord*8] {pst & ls.selectedPiece =  gs.worldMatrix.[7+mouseUpyCord*8]}
	castleRight =  disableBothCastle moveRookLeft
	moveRookRight = MovePiece (0,mouseUpyCord) 3 mouseUpyCord gs.worldMatrix.[0+mouseUpyCord*8] {pst & ls.selectedPiece =  gs.worldMatrix.[0+mouseUpyCord*8]}
	castleLeft = disableBothCastle moveRookRight 
