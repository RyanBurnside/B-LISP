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
    temp = u_longptr[0] & $ffffffffffff:ULong
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

' interning of atom names (Lisp symbols), returns a unique NaN-boxed ATOM
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

' construct pair (x . y) returns a NaN-boxed CONS_TAG
Function cons:Double(x:Double, y:Double)
    sp :- 1
    cell[sp] = x
    sp :- 1
    cell[sp] = y
    If (hp > sp Shl 3) Then reset()
    Return box(CONS_TAG, sp)
End Function

' return the car of the pair or ERR if not a pair
Function car:Double(p:Double) ' check
    If T(p) & ~(CONS_TAG ~ CLOS_TAG) = CONS_TAG
        Return cell[ord(p) + 1]
    End If
    Return err_val 
End Function

' return the cdr of the pair or ERR if not a pair
Function cdr:Double(p:Double) ' check
    If T(p) & ~(CONS_TAG ~ CLOS_TAG) = CONS_TAG
        Return cell[ord(p)]
    End If
    Return err_val 
End Function

' construct a pair to add to environment e, returns the list ((v . x) . e)
Function pair:Double(v:Double, x:Double, e:Double) ' check
    Return cons(cons(v, x), e)
End Function

' construct a closure, returns a NaN-boxed CLOS_TAG
Function closure:Double(v:Double, x:Double, e:Double) ' check
    '  return box(CLOS,ord(pair(v,x,equ(e,env) ? nil : e))); }
    Local env:Double
    If equ(e, env_val) Then env = nil_val Else env = e
    Return box(CLOS_TAG, ord(pair(v, x, env)))
End Function

' look up a symbol in an environment, return its value or err if not found
Function assoc:Double(v:Double, e:Double)
    While T(e) = CONS_TAG And Not equ(v, car(car(e)))
        e = cdr(e)
    Wend
    If T(e) = CONS_TAG Then Return cdr(car(e)) Else Return err_val
End Function

' lispNot(x) is nonzero if x is the lisp () empty list
Function lispNot:ULong(x:Double)
    Return T(x) = NIL_TAG
End Function

' let(x) is nonzero if x is a Lisp let/let* pair
Function let:ULong(x:Double)
    Return T(x) <> NIL_TAG  And (Not lispNot(cdr(x)))
End Function

' return a new list of evaluated Lisp expresions t in the environment e
Function evlis:Double(tag:Double, e:Double)
    Select T(tag)
    Case CONS_TAG 
        Return cons(eval(car(tag), e), evlis(cdr(tag), e))
    Case ATOM_TAG 
        Return assoc(tag, e)
    Default 
        Return nil_val
    End Select
End Function

' Lisp primitives:
'   (eval x)            return evaluated x (such as when x was quoted)
'   (quote x)           special form, returns x unevaluated "as is"
'   (cons x y)          construct pair (x . y)
'   (car p)             car of pair p
'   (cdr p)             cdr of pair p
'   (add n1 n2 ... nk)  sum of n1 to nk
'   (sub n1 n2 ... nk)  n1 minus sum of n2 to nk
'   (mul n1 n2 ... nk)  product of n1 to nk
'   (div n1 n2 ... nk)  n1 divided by the product of n2 to nk
'   (int n)             integer part of n
'   (< n1 n2)           #t if n1<n2, otherwise ()
'   (eq? x y)           #t if x equals y, otherwise ()
'   (not x)             #t if x is (), otherwise ()
'   (or x1 x2 ... xk)   first x that is not (), otherwise ()
'   (and x1 x2 ... xk)  last x if all x are not (), otherwise ()
'   (cond (x1 y1)
'         (x2 y2)
'         ...
'         (xk yk))      the first yi for which xi evaluates to non-()
'   (if x y z)          if x is non-() then y else z
'   (let* (v1 x1)
'         (v2 x2)
'         ...
'         y)            sequentially binds each variable v1 to xi to evaluate y
'   (lambda v x)        construct a closure
'   (define v x)        define a named value globally

Function f_eval:Double(tt:Double, e:Double)
    Return eval(car(evlis(tt, e)), e)
End Function

Function f_quote:Double(tt:Double, _:Double)
    Return car(tt)
End Function

Function f_cons:Double(tt:Double, e:Double)
    Return cons(car(tt), car(cdr(tt)))
End Function

Function f_car:Double(tt:Double, e:Double)
    Return car(car(evlis(tt, e)))
End Function

Function f_cdr:Double(tt:Double, e:Double)
    Return cdr(car(evlis(tt, e)))
End Function

Function f_add:Double(tt:Double, e:Double)
    tt = evlis(tt, e)
    If T(tt) = NIL_TAG Then Return num(0)
    Local n:Double = 0
    While Not lispNot(tt)
        n :+ car(tt)
        tt = cdr(tt)
    Wend
    Return num(n)
End Function

Function f_sub:Double(tt:Double, e:Double)
    'tt = evlis(tt, e)
    Local n:Double = car(tt)
    If T(tt) = NIL_TAG Then Return err_val
    If T(cdr(tt)) = NIL_TAG Then Return num(-n)
    While Not lispNot(tt)
        tt = cdr(tt)
        n :- car(tt)
    Wend
    Return num(n)
End Function

Function f_mul:Double(tt:Double, e:Double)
    tt = evlis(tt, e)
    If T(tt) = NIL_TAG Then Return num(1)
    Local n:Double = 1
    While Not lispNot(tt)
        n :* car(tt)
        tt = cdr(tt)
    Wend
    Return num(n)
End Function

Function f_div:Double(tt:Double, e:Double)
    tt = evlis(tt, e)
    Local n:Double = car(tt)
    If T(tt) = NIL_TAG Then Return err_val
    If T(cdr(tt)) = NIL_TAG Then Return num(1.0 / n)
    While Not lispNot(tt)
    Print ">" + n
        n :/ car(tt)
        tt = cdr(tt)
    Wend
    Return num(n)
End Function

Function f_int:Double(t:Double, e:Double)
    Local n:Double = car(evlis(t,e))
    If n < 1e16 And n > 1e-16 Then Return Long(n)
    Return n
End Function

Function f_lt:Double(t:Double, e:Double)
    t = evlis(t, e)
    If car(t) - car(cdr(t)) < 0 Then Return tru_val
    Return nil_val
End Function

Function f_eq:Double(t:Double, e:Double)
    t = evlis(t, e)
    If equ(car(t), car(cdr(t))) Then Return tru_val
    Return nil_val
End Function

Function f_not:Double(t:Double, e:Double)
    If lispNot(car(evlis(t, e))) Then Return tru_val
    Return nil_val
End Function

Function f_or:Double(tt:Double, e:Double)
    Local x:Double = nil_val
    While T(tt) <> NIL_TAG
        x = eval(car(tt), e)
        If Not lispNot(x) Then Return x
        tt = cdr(tt) 
    Wend
    Return x    
End Function

Function f_and:Double(tt:Double, e:Double) ' double check
    Local x:Double = nil_val
    While T(tt) <> NIL_TAG
        x = eval(car(tt), e)
        If lispNot(x) Then Exit
        tt = cdr(tt)
    Wend
    Return x
End Function

Function f_cond:Double(tt:Double, e:Double) ' double check
    While T(tt) <> NIL_TAG And lispNot(eval(car(car(tt)), e))
        tt = cdr(tt)
    Wend
    Return eval(car(cdr(car(tt))), e)
End Function

Function f_if:Double(tt:Double, e:Double) ' double check
    Local pred:Double = lispNot(eval(car(tt), e))
    Local answer:Double = tt
    If pred Then answer = cdr(tt)
    Return eval(car(cdr(answer)), e)
End Function

Function f_leta:Double(tt:Double, e:Double)
    While let(tt)
        e = pair(car(car(tt)), eval(car(cdr(car(tt))), e), e)
        tt = cdr(tt)
    Wend
    Return eval(car(tt), e)
End Function

Function f_lambda:Double(tt:Double, e:Double) ' double check
    Return closure(car(tt), car(cdr(tt)), e)
End Function

Function f_define:Double(tt:Double, e:Double)
    env_val = pair(car(tt), eval(car(cdr(tt)), e), env_val)
    Return car(tt)
End Function

' table of Lisp primitives, each has a name s And a Function pointer f
Type fnPointer
    Field s:String
    Field f:Double(tt:Double, e:Double)
    Method New (s:String, f:Double(tt:Double, e:Double))
        Self.s = s
        Self.f = f
    End Method
End Type

Global prim:fnPointer[] = [ ..
New fnPointer("eval",   f_eval),
New fnPointer("quote",  f_quote),
New fnPointer("cons",   f_cons),
New fnPointer("car",    f_car),
New fnPointer("cdr",    f_cdr),
New fnPointer("+",      f_add),
New fnPointer("-",      f_sub),
New fnPointer("*",      f_mul),
New fnPointer("/",      f_div),
New fnPointer("int",    f_int),
New fnPointer("<",      f_lt),
New fnPointer("eq?",    f_eq),
New fnPointer("or",     f_or),
New fnPointer("and",    f_and),
New fnPointer("not",    f_not),
New fnPointer("cond",   f_cond),
New fnPointer("if",     f_if),
New fnPointer("let*",   f_leta),
New fnPointer("lambda", f_lambda),
New fnPointer("define", f_define)]

' create environment by extending e with the variables v bount to values t

Function bind:Double(v:Double, tt:Double, e:Double)
    Select T(v)
    Case NIL_TAG Return e
    Case CONS_TAG Return bind(cdr(v), cdr(tt), pair(car(v), car(tt), e))
    Default Return pair(v, tt, e)
    End Select
End Function

Function reduce:Double(f:Double, tt:Double, e:Double)
    Local en:Double = cdr(f)
    If lispNot(cdr(f)) Then en = env_val
    Return eval(cdr(car(f)), bind(car(car(f)), evlis(tt, e), en))
End Function 

Function Apply:Double(f:Double, tt:Double, e:Double)
    Select T(f)
    Case PRIM_TAG Return prim[ord(f)].f(tt, e)
    Case CLOS_TAG Return reduce(f, tt, e)
    Default Return err_val
    End Select
End Function

Function eval:Double(x:Double, e:Double)
    Select T(x)
    Case ATOM_TAG Return assoc(x, e)
    Case CONS_TAG Return Apply(eval(car(x), e), cdr(x), e)
    Default Return x
    End Select
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

Function eval:Double(a:Double, b:Double)
    Print "***Eval not yet implemented, giving wrong answer!***"
End Function

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
