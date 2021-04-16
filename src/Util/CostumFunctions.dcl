definition module Util.CostumFunctions

import StdEnv, StdIO


/// Applying a recursive sequence of functions on unique types from an array.
seqArray :: !{*s -> *s} *s -> *s
