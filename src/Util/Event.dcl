definition module Util.Event

import StdEnv, StdIO, Util.Constants

/// handling mouse events
mouseHandler :: MouseState (.ls, *PSt GameState) -> (.ls,*PSt GameState)

/// filtering out the unwanted mouse events
m_filter :: MouseState -> Bool  

/// saving game state and quiting
quit:: (.ls, *PSt .l) -> (.ls, *PSt .l)