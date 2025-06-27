SuperStrict

' This module follows much of the TinyLisp (99 lines of C) implementation
' https://github.com/Robert-van-Engelen/tinylisp
' Also it takes some inspriation from the 1000 line extension
' https://github.com/Robert-van-Engelen/lisp

Import brl.retro
Import text.format
Import Text.RegEx

Include "primitives.bmx"

Type Blisp
    ' NaN box constant "tags" For encoding into doubles
    Const ATOM_TAG:ULong = $7ff8  'atom      1111111111111 000
    Const PRIM_TAG:ULong = $7ff9  'primitive 1111111111111 001
    Const CONS_TAG:ULong = $7ffa  'cons cell 1111111111111 010
    Const CLOS_TAG:ULong = $7ffb  'closure   1111111111111 011
    Const NIL_TAG:ULong  = $7ffc  'duh       1111111111111 100
    
    Field N:UInt =  1024 * 1024 * 5 ' number of Lisp objects (doubles) to store
    Field hp:ULong = 0 ' heap pointer
    Field sp:ULong = N ' stack pointer
    Field cell:Double[N]
    
    ' These will get populated in main()
    ' They stand For literal values
    Field  nil_val:Double
    Field  quit_val:Double
    Field  tru_val:Double
    Field  err_val:Double
    Field  env_val:Double
    
    ' --------------------------------------------------------------------
    ' This is a table of primitive functions called by symbol lookup
    ' --------------------------------------------------------------------
    ' Given a symbol Return the Method it represents
    Field prim:fnPointer[] = Null

    Method New()
        Self.prim = makeFullFnTable(Self)
    End Method

    Method New(fnTbl:fnPointer[])
        Self.prim = fnTbl
    End Method

    ' Set all the doubles To 0 in the machine
    Method resetCells()
        For Local i:Int = 0 Until n
            cell[i] = 0
        Next
    End Method

    ' Reset the state of the machine And initialize environment
    Method resetMachine()
        resetCells()

        nil_val = box(NIL_TAG, 0)                 ' TODO make roots in real GC
        quit_val = box(NIL_TAG, 1)                ' TODO make roots in real GC
        err_val = atom("ERR")                     ' TODO make roots in real GC
        tru_val = atom("t")                       ' TODO make roots in real GC
        env_val = pair(tru_val, tru_val, nil_val) ' TODO make roots in real GC

        ' Populate the environment with the primitives from the table
        For Local i:ULong = 0 Until prim.Length
            Print "Adding a primitive: " + prim[i].s
            env_val = pair(atom(prim[i].s), box(PRIM_TAG, i), env_val)
        Next

    End Method


    ' This stupid thing needs to exist because blitzmax
    ' mandates newlines with Print
    Method prin(s:String)
        StandardIOStream.WriteString s
        StandardIOStream.Flush ()
    End Method

    ' Printable versions of our NaN boxing constants
    Method tagToString:String(tag:ULong)
        Select tag
            Case ATOM_TAG Return "ATOM"
            Case PRIM_TAG Return "PRIMITIVE"
            Case CONS_TAG Return "CONS"
            Case CLOS_TAG Return "CLOSURE"
            Case NIL_TAG Return "NIL"
            Default Return "UNKNOWN TYPE"
        End Select
    End Method

    ' We want to force the Lisp expression (double) into being read as an unsigned int
    ' this allows us to shift the bits and find the tag it was encoded with
    Method getTag:ULong(x:Double)
        'GCSuspend()
        Local ulong_ptr:ULong Ptr = Varptr x
        Local result:ULong = ulong_ptr[0] Shr 48
        'GCResume()
        Return result
    End Method

    ''' NaN-boxing specific functions:
    '''    box(t,i): returns a New NaN-boxed Double with tag t And ordinal i
    Method box:Double(tag:ULong, i:ULong)
        Local temp:Double
        'GCSuspend()
        Local u_longptr:ULong Ptr = Varptr temp
        u_longptr[0] = tag Shl 48 | i
        'GCResume()
        Return temp
    End Method

    ''' ord(x): returns the ordinal of the NaN-boxed Double x
    ''' not representative of *actual* value in base 10
    Method ord:ULong(x:Double) 
        Local temp:ULong
        'GCSuspend()
        Local u_longptr:ULong Ptr = Varptr x
        temp = u_longptr[0] & $ffffffffffff:ULong
        'GCResume()
        Return temp
    End Method

    ' Does nothing, but could be extended to check for NaN
    Method num:Double(n:Double)
        Return n
    End Method

    ' Returns nonzero if x equals y
    Method equ:ULong(x:Double, y:Double)
        Local temp:ULong
        'GCSuspend()
        Local u_longptr_x:ULong Ptr = Varptr x
        Local u_longptr_y:ULong Ptr = Varptr y
        temp = (u_longptr_x[0] = u_longptr_y[0])
        'GCResume()
        Return temp
    End Method

    ' interning of atom names (Lisp symbols), returns a unique NaN-boxed ATOM
    Method atom:Double(s:String)
        Local i:ULong = 0
        GCSuspend()
        Local charPtr:Byte Ptr = cell
        Local embeddedStr:String = String.FromCString(charPtr)
        While i < hp And embeddedStr <> s
            i :+ embeddedStr.Length + 1
            embeddedStr = String.FromCString(charPtr + i)
        Wend

        If i = hp
            Local s_size:Size_T = s.Length + 1
            hp :+ s_size
            If hp > (sp Shl 3)
                Print "B-LISP: Critical Error! Heap pointer greater than stack pointer!"
            EndIf
            s.ToUTF8StringBuffer(charPtr + i, s_size)
        EndIf
        GCResume()
        Return box(ATOM_TAG, i)
    End Method

    ' construct pair (x . y) returns a NaN-boxed CONS_TAG
    Method cons:Double(x:Double, y:Double)
        sp :- 1
        cell[sp] = x
        sp :- 1
        cell[sp] = y
        Return box(CONS_TAG, sp)
    End Method

    ' return the car of the pair or ERR if not a pair
    Method car:Double(p:Double)
        If lispNot(p) Return nil_val ' modified

            If getTag(p) & ~(CONS_TAG ~ CLOS_TAG) = CONS_TAG
                Return cell[ord(p) + 1]
            End If
            Return err_val
    End Method

    ' return the cdr of the pair or ERR if not a pair
    Method cdr:Double(p:Double) 
        If lispNot(p) Return nil_val ' modified

            If getTag(p) & ~(CONS_TAG ~ CLOS_TAG) = CONS_TAG
                Return cell[ord(p)]
            End If
            Return err_val
    End Method

    ' Since car(cdr(...)) is used so much, let's just define 'second'
    Method second:Double(p:Double)
        Return car(cdr(p))
    End Method

    Method third:Double(p:Double)
        Return car(cdr(cdr(p)))
    End Method

    Method fourth:Double(p:Double)
        Return car(cdr(cdr(cdr(p))))
    End Method

    ' construct a pair to add to environment e, returns the list ((v . x) . e)
    Method pair:Double(v:Double, x:Double, e:Double)
        Return cons(cons(v, x), e)
    End Method

    ' construct a closure, returns a NaN-boxed CLOS_TAG
    Method closure:Double(v:Double, x:Double, e:Double)
        Local env:Double
        If equ(e, env_val) Then env = nil_val Else env = e
        Return box(CLOS_TAG, ord(pair(v, x, env)))
    End Method

    ' look up a symbol in an environment, return its value or err if not found
    ' This is different behavior than Common Lisp which returns (key value)
    ' returns value For dotted pairs, returns (value) for lists pairs.
    ' this is a poorly named system level thing, not a repl thing
    Method assoc:Double(v:Double, e:Double)
        While getTag(e) = CONS_TAG And (Not equ(v, car(car(e))))
            e = cdr(e)
        Wend
        If getTag(e) = CONS_TAG Then Return cdr(car(e))
        Return err_val
    End Method

    ' lispNot(x) is nonzero if x is the lisp () empty list
    Method lispNot:ULong(x:Double)
        Return getTag(x) = NIL_TAG
    End Method

    Method let:ULong(x:Double)
        Return getTag(x) <> NIL_TAG And getTag(cdr(x)) <> NIL_TAG
    End Method

    Method evlis:Double(t:Double, e:Double)
        Local s:Double = nil_val
        GCSuspend()
        Local p:Double Ptr = Varptr s
        Local array_ptr:Double Ptr = Varptr cell
        While getTag(t) = CONS_TAG
            p[0] = cons(eval(car(t), e), nil_val)
            p = array_ptr + sp
            t = cdr(t)
        Wend
        If getTag(t) = ATOM_TAG Then p[0] = assoc(t, e)
        GCResume()
        Return s
    End Method

    ' --------------------------------------------------------------------
    ' these f_ prefixed functions get called in the Function lookup table
    ' t is a parameters list, e is the Global environment
    ' --------------------------------------------------------------------

    ' Since assoc above is a system thing, this is the proper abstraction for Lisp
    Method lisp_assoc:Double(v:Double, e:Double)
        While getTag(e) = CONS_TAG And (Not equ(v, car(car(e))))
            e = cdr(e)
        Wend
        If getTag(e) = CONS_TAG Then Return car(e)
        Return nil_val
    End Method

    Method notImpYet(killLisp:Int = 0, msg:String = "")
        Print msg
        If killLisp
            Print "You are returning To the BlitzMax shell, press q hit Enter."
            Throw "Exiting B-LISP."
        End If
    End Method

    ' create environment by extending e with the variables v bount to values t
    Method bind:Double(v:Double, t:Double, e:Double)
        Select getTag(v)
            Case NIL_TAG Return e
            Case CONS_TAG Return bind(cdr(v), cdr(t), pair(car(v), car(t), e))
            Default Return pair(v, t, e)
        End Select
    End Method

    Method reduce:Double(f:Double, t:Double, e:Double)
        Local en:Double = cdr(f)
        If lispNot(cdr(f)) Then en = env_val
        Return eval(cdr(car(f)), bind(car(car(f)), evlis(t, e), en))
    End Method

    Method Apply:Double(f:Double, t:Double, e:Double)
        Select getTag(f)
            Case PRIM_TAG Return prim[ord(f)].f(Self, t, e) ' look up in prim array
            Case CLOS_TAG Return reduce(f, t, e) '
            Default Return err_val
        End Select
    End Method

    Method eval:Double(x:Double, e:Double)
        Select getTag(x)
            Case ATOM_TAG Return assoc(x, e)
            Case CONS_TAG Return Apply(eval(car(x), e), cdr(x), e)
            Default Return x
        End Select
    End Method

    Method printlist(t:Double)
        prin "("
        Repeat
            lispPrint car(t)
            t = cdr(t)
            If getTag(t) = NIL_TAG Then Exit
            If getTag(t) <> CONS_TAG
                prin " . "
                lispPrint(t)
                Exit
            End If
            prin " "
        Forever
        prin ")"
    End Method

    Method lispPrint(x:Double)
        Select getTag(x)
            Case NIL_TAG prin "()"
            Case ATOM_TAG
                GCSuspend()
                Local A:Byte Ptr = cell
                prin "".FromCString(A+ord(x))
                GCResume()
            Case PRIM_TAG prin "PRIM-FN<" + prim[ord(x)].s + ">"
            Case CONS_TAG printlist(x)
            Case CLOS_TAG prin "CLOSURE<" + ULong(ord(x)) + ">"
            Default
                Local F:TFormatter = New TFormatter.Create("%.10f") ' test
                prin F.arg(x).format()
        End Select
    End Method

    Method apiPrintlist(t:Double)
        Local listLength:ULong = 0
        Repeat
            listLength:ULong :+ 1
            prin "cons("
            apiPrint car(t)
            
            t = cdr(t)
            If getTag(t) = NIL_TAG
                prin ", "
                apiPrint(t)
                Exit
            End If 
            If getTag(t) <> CONS_TAG
                prin ", "
                apiPrint(t)
                Exit
            End If
            prin ", "
        Forever
        For Local i:ULong = 0 Until listLength
            prin ")"
        Next
    End Method

    Method apiPrint(x:Double)
        Select getTag(x)
            Case NIL_TAG prin "nil_val"
            Case ATOM_TAG
                GCSuspend()
                Local A:Byte Ptr = cell
                prin "atom(~q" + "".FromCString(A+ord(x)) + "~q)"
                GCResume()
            Case PRIM_TAG
                GCSuspend()
                Local A:Byte Ptr = cell
                prin "prim[ord(atom(~q" + prim[ord(x)].s + "~q))].s"
                GCResume()
            Case CONS_TAG apiPrintlist(x)
            Case CLOS_TAG
                notImpYet(0, "~qNot implemented yet: " + ..
                "apiPrint(): CLOSURE<" + ULong(ord(x)) + ">~q")
            Default
                Local F:TFormatter = New TFormatter.Create("%.10f") ' test
                prin "num(" + F.arg(x).format() + ")"
        End Select
    End Method


    ' Yeah, this isn't going to stick around Forever. :D
    ' Method gc()
    '     sp = ord(env_val)
    ' End Method

    Method gc()
        sp = ord(env_val)

        For Local i:Double = EachIn cell
            If getTag(i) = ATOM_TAG And ord(i) > hp Then hp = ord(i)
        Next

        GCSuspend()
        Local A:Byte Ptr = cell
        hp :+ String.fromCString(A + hp).Length + 1
        GCResume()
    End Method

    Method debugPrint(x:Double)
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
        Print "Sign bit:" + String.FromULong(val Shr 63)
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
    End Method

    Method dump()
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
    End Method

    Method test_lambda:Double(t:Double, e:Double)
        Local parsed:Double = evlis(t, e) ' meh optimize in future
        ' pray For BlitzMax
        ' (lambda (i) (lambda (x) (+ x i)))
        Local inpt:Double = cons(atom("lambda"), cons(cons(atom("i"), nil_val),
        cons(cons(atom("lambda"),
        cons(cons(atom("x"), nil_val),
        cons(cons(atom("+"), cons(atom("x"), cons(atom("i"),
        nil_val))), nil_val))), nil_val)))
        Local answer:Double = num(30)
        
        Local generator:Double = eval(inpt, e)
        Local clos:Double = eval(cons(generator, cons(num(10.0), nil_Val)), e)
        Local ret:Double = eval(cons(clos, cons(num(20.0), nil_Val)), e)
        Local passed:Double = f_eqNum(Self, cons(ret, cons(answer, nil_val)), e)
        Return passed
    End Method

    Method test_assoc:Double(t:Double, e:Double)
        Local Input:Double = cons(atom("assoc"), cons(cons(atom("quote"), cons(cons(cons(atom("a"), cons(atom("b"), nil_val)), cons(cons(atom("c"), cons(atom("d"), nil_val)), nil_val)), nil_val)), cons(cons(atom("quote"), cons(atom("a"), nil_val)), nil_val)))

    End Method

End Type
