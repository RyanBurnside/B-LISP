' This module follows much of the TinyLisp (99 lines of C) implementation
SuperStrict
Import brl.retro

' The following is a bit of a hack
' We want to force the Lisp expression (double) into being misread as an unsighed int
' this allows us to shift the bits and find the tag it was encoded with
Function T:ULong(x:Double) Inline
	GCSuspend()
	Local ulong_ptr:ULong Ptr = Varptr x
	Local result:ULong = ulong_ptr[0] Shr 48
	GCResume()
	Return result
End Function

Global N:UInt = 4096 ' number of Lisp objects (doubles) to store for the VM

Global hp:ULong = 0 ' heap pointer
Global sp:ULong = N ' stack pointer

Global cell:Double[N]

' NaN box constant "tags"
Const ATOM:ULong = $7ff8  'atom
Const PRIM:ULong = $7ff9  'primitive
Const CONS:ULong = $7ffa  'cons cell
Const CLOS:ULong = $7ffb  'closure
Const NIL:ULong  = $7ffc  'duh

Global nil_val:Double ' WHEN DO THESE GET ASSIGNED!?
Global tru_val:Double 
Global err_val:Double
Global env_val:Double

''' NaN-boxing specific functions:
'''    box(t,i): returns a New NaN-boxed Double with tag t And ordinal i
Function box:Double(tag:ULong, i:ULong) Inline
	Local temp:Double
	GCSuspend()
	Local u_longptr:ULong Ptr = Varptr temp
	u_longptr[0] = tag Shl 48 | i
	GCResume()
	Return temp
End Function

''' ord(x): returns the ordinal of the NaN-boxed Double x
''' not representative of *actual* value in base 10 
Function ord:ULong(n:Double) Inline
	Local temp:ULong	
	GCSuspend()
	Local u_longptr:ULong Ptr = Varptr n
	temp = u_longptr[0]
	GCResume()
	Return temp
End Function 



