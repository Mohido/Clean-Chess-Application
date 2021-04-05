module pawn
import StdEnv

import Pices.main_pieces

/*
My posibilities for the pawn: 
	Normally move forward one space.
	Move diagonal one space to kill.
	May move two spaces forward on the first move.
		But may be killed En Passant after this move by attacking the space behind the pawn.
	May be exchanged for any piece except a King by reaching the opposite side of the board.
*/

Start = 9 
MovePawn :: Piece Int Int Piece (*PSt .l) -> (Piece, (*PSt .l))
MovePawn enemy toXPos toYPos pawn pst = killPawn enemy xPos yPos pawn pst
	
	
killPawn :: Piece Int Int Piece (*PSt .l) -> (Piece, (*PSt .l))
killPawn enemy toXPos toYPos pawn pst
	# movedPawnRight = { x = x + 1 , y = y + 1 , chessPiece = chessPiece , colour = colour} 
	# movedPawnLeft = { x = x + 1 , y = y - 1 , chessPiece = chessPiece , colour = colour} 
	| (toXPos == enemy.x && toXPos + 1 == enemy.y) = (movedPawnRight, pst)
	| (toXPos == enemy.x && toXPos - 1 == enemy.y) = (movedPawnLeft, pst)
	| enemy.chessPiece == NotPiece && pawn.colour == BlackPiece = MoveBlack xPos yPos pawn pst
	| enemy.chessPiece == NotPiece && pawn.colour == WhitePiece = MoveWhite xPos yPos pawn pst
	| enemy.chessPiece == King && pawn.colour == BlackPiece = MoveBlack xPos yPos pawn pst
	| enemy.chessPiece == King && pawn.colour == WhitePiece = MoveWhite xPos yPos pawn pst
	
MoveBlack :: Int Int Piece (*PSt .l) -> (Piece, (*PSt .l))
MoveBlack toXPos toYPos pawn pst
	# movedOneStepPawn = { x = x + 1 , y = y , chessPiece = chessPiece , colour = colour}
	# movedOneTwoPawn = { x = x + 2 , y = y , chessPiece = chessPiece , colour = colour}
	| toXpos == pawn.x + 1 && toYPos == pawn.y = (movedOneStepPawn , pst)
	| toXpos == pawn.x + 2 && toYPos == pawn.y = (movedOneTwoPawn , pst)
	= (pawn, pst)
	
MoveWhite :: Int Int Piece (*PSt .l) -> (Piece, (*PSt .l))
MoveWhite toXPos toYPos pawn pst
	# movedOneStepPawn = { x = x - 1 , y = y , chessPiece = chessPiece , colour = colour}
	# movedOneTwoPawn = { x = x - 2 , y = y , chessPiece = chessPiece , colour = colour}
	| toXpos == pawn.x - 1 && toYPos == pawn.y = (movedOneStepPawn , pst)
	| toXpos == pawn.x - 2 && toYPos == pawn.y = (movedOneTwoPawn , pst)
	= (pawn, pst)
	