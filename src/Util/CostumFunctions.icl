implementation module Util.CostumFunctions

import StdEnv, StdIO

seqArray :: !{*s -> *s} *s -> *s
seqArray arr param = auxSeqArray arr param 0
where
	auxSeqArray :: !{*s -> *s} *s Int -> *s
	auxSeqArray arr param ind
	| ind >= size (arr) = param
	#! param = (arr.[ind] param)
	= auxSeqArray arr param (ind + 1)