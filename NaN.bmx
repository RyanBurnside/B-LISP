' This module follows much of the TinyLisp (99 lines of C) implementation
SuperStrict
Import brl.retro
Import text.format

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

Global N:UInt = 1024 ' number of Lisp objects (doubles) to store for the VM

Global hp:ULong = 0 ' heap pointer
Global sp:ULong = N ' stack pointer

Global cell:Double[N]

Function reset()
	Print "B-LISP: Resetting heap and stack,"
	Print "B-LIsp: Using " + String(N) + " Cells."
	hp = 0 ' heap pointer
	sp = N ' stack pointer
	cell = New Double[N]
End Function

' Reset
reset()

' NaN box constant "tags"
Const ATOM_TAG:ULong = $7ff8  'atom
Const PRIM_TAG:ULong = $7ff9  'primitive
Const CONS_TAG:ULong = $7ffa  'cons cell
Const CLOS_TAG:ULong = $7ffb  'closure
Const NIL_TAG:ULong  = $7ffc  'duh

Function tagToString:String(tag:ULong)
	Select tag
		Case ATOM_TAG Return "ATOM"
		Case PRIM_TAG Return "PRIMITIVE"
		Case CONS_TAG Return "CONS"
		Case CLOS_TAG Return "CLOSURE"
		Case NIL_TAG Return "NIL"
		Default Return "UNKNOWN TYPE"
	End Select
End Function

Global nil_val:Double ' Ask Pete about these...
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

' Does nothing, but could be extended to check for NaN
Function num:Double(n:Double) Inline
	Return n
End Function

' Returns nonzero if x equals y
Function equ:ULong(x:Double, y:Double) Inline
	Local temp:ULong
	GCSuspend()
	Local u_longptr_x:ULong Ptr = Varptr x
	Local u_longptr_y:ULong Ptr = Varptr y
	temp = (u_longptr_x[0] = u_longptr_y[0])
	GCResume()
	Return temp
EndFunction

Function atom:Double(s:String)
	Local i:ULong = 0
	GCSuspend()
	Local charPtr:Byte Ptr = cell
	Local embeddedStr:String = String.FromCString(charPtr + i)
	
	While i < hp And embeddedStr <> s
		embeddedStr = String.FromCString(charPtr + i)
		i :+ embeddedStr.Length + 1 ' we will store as null terminated C string
	Wend
	
	If i = hp
		Local s_size:Size_T = s.Length + 1
		hp :+ s_size
		s.ToUTF8StringBuffer(charPtr + i, s_size)
		Print s
		If hp > (sp Shl 3)
			Print "B-LISP: Critical Error! Heap pointer greater than stack pointer!"
			reset()
		EndIf
	EndIf
	GCResume()
	Return box(ATOM_TAG, i)
End Function

Function cons:Double(x:Double, y:Double)
	sp :- 1
	cell[sp] = x
	sp :- 1
	cell[sp] = y
	If (hp > sp Shl 3) Then reset()
	Return box(CONS_TAG, sp)
End Function

Function car:Double(p:Double) ' check
	If T(p) & ~(CONS_TAG ~ CLOS_TAG) = CONS_TAG
		Return cell[ord(p) + 1]
	Else
		Return err_val
	End If 
End Function

Function cdr:Double(p:Double) ' check
	If T(p) & ~(CONS_TAG ~ CLOS_TAG) = CONS_TAG
		Return cell[ord(p)]
	Else
		Return err_val
	End If 
End Function

Function pair:Double(v:Double, x:Double, e:Double) ' check
	Return cons(cons(v, x), e)
End Function

Function closure:Double(v:Double, x:Double, e:Double) ' check
	'  return box(CLOS,ord(pair(v,x,equ(e,env) ? nil : e))); }
	Local env:Double
	If equ(e, env_val) Then env = nil_val Else env = e
	Return box(CLOS_TAG, ord(pair(v, x, env)))
End Function

Function debugPrint(x:Double)
	Local val:ULong
	Local mask:ULong = $ff
	
	GCSuspend()
	Local u_longptr:ULong Ptr = Varptr x
	val = u_longptr[0]
	GCResume()
	
	mask :Shl 56
	
	Print "*** Debug Print ***"
	Print "Analyzed double"
	Print "Original double value: " + String.FromDouble(x)
	Print "Sign bit:" + String.FromLong(val Shr 63)
	Print "Exponent: $" + Hex((val Shr 52) & $7FF)[5..]
	Print "Mantissa: $" + LongHex(val & $FFFFFFFFFFFFF:ULong)[3..]
	If x = x Then Print "NaN: no" Else Print "NaN: yes" 
	
	Print "Raw Bytes From MSB to LSB"
	Local numBytes:Int = SizeOf(val)
	Local strBuff:String[] = New String[numBytes]
	For Local i:Int = 0 Until numBytes
		strBuff[i] = Hex((val & mask) Shr ((numBytes - 1 - i) * 8))[6..]
        mask :Shr 8;
	Next 
	Print "$"+" ".Join(strBuff)
	
	Print "Individual Bits"
	mask = ULong(1) Shl 63
	Local bitBuffer:String = ""
	Local labelBuffer:String = ""
	For Local i:Int = 0 Until numBytes * 8
		If (i = 1 Or i = 12) ' boundary spaces
			bitBuffer :+ " "
			labelBuffer :+ " "
		End If
		
		If (val & mask) bitBuffer :+ "1" Else bitBuffer :+ "0"
		mask :Shr 1
		
		' label buffer updates
		If i = 0 
            labelBuffer :+ "s"
        Else If i > 0 And i < 12
            labelBuffer :+ "e"
        Else
            labelBuffer :+ "m"
        End If
		
	Next
	Print labelBuffer
	Print bitBuffer
	
	
	Print "TinyLisp Representation"
	Local tagVal:ULong = val Shr 48
	
	Print "Tag Bits: $" + Hex(tagVal)[4..] + " (" + tagToString(tagVal) + ")"
	Print "Storage Bits: $" + LongHex(val & $FFFFFFFFFFFF:ULong)
	Print "*** end debug ***~n"
End Function

For Local test:String = EachIn ["foo", "foo", "foo"]
	atom(test)
Next

Function dump()
	GCSuspend()
	Local p:Byte Ptr = cell
	Local buffer:String = ""
	For Local i:Int = 0 Until cell.Length * 8
		If i Mod 40 = 0 Then buffer :+ "~n"
		If p[0] <> 0 
			buffer :+ Chr(p[0]) + " "
		Else 
			buffer :+ ". "
		End If
		p :+ 1
	Next
	Print buffer
	GCResume()
End Function

dump()
