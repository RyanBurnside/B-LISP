SuperStrict

' This module follows much of the TinyLisp (99 lines of C) implementation
' https://github.com/Robert-van-Engelen/tinylisp
' Also it takes some inspriation from the 1000 line extension
' https://github.com/Robert-van-Engelen/lisp

Import brl.retro
Import text.format
Import Text.RegEx

' NaN box constant "tags" For encoding into doubles
Const ATOM_TAG:ULong = $7ff8  'atom      1111111111111 000
Const PRIM_TAG:ULong = $7ff9  'primitive 1111111111111 001
Const CONS_TAG:ULong = $7ffa  'cons cell 1111111111111 010
Const CLOS_TAG:ULong = $7ffb  'closure   1111111111111 011
Const NIL_TAG:ULong  = $7ffc  'duh       1111111111111 100

Global N:UInt =  1024 * 1024 * 5 ' number of Lisp objects (doubles) to store
Global hp:ULong = 0 ' heap pointer
Global sp:ULong = N ' stack pointer
Global cell:Double[N]

' These will get populated in main()
' They stand For literal values
Global nil_val:Double
Global quit_val:Double
Global tru_val:Double
Global err_val:Double
Global env_val:Double

' Set all the doubles To 0 in the machine
Function resetCells()
    For Local i:Int = 0 Until n
        cell[i] = 0
    Next
End Function

' Reset the state of the machine And initialize environment
Function resetMachine()
    resetCells()

    nil_val = box(NIL_TAG, 0)                 ' TODO make roots in real GC
    quit_val = box(NIL_TAG, 1)                ' TODO make roots in real GC
    err_val = atom("ERR")                     ' TODO make roots in real GC
    tru_val = atom("t")                       ' TODO make roots in real GC
    env_val = pair(tru_val, tru_val, nil_val) ' TODO make roots in real GC

    For Local i:ULong = 0 Until prim.Length
        env_val = pair(atom(prim[i].s), box(PRIM_TAG, i), env_val)
    Next

End Function


' This stupid thing needs to exist because blitzmax
' mandates newlines with Print
Function prin(s:String)
    StandardIOStream.WriteString s
    StandardIOStream.Flush ()
End Function

' Printable versions of our NaN boxing constants
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

' We want to force the Lisp expression (double) into being read as an unsighed int
' this allows us to shift the bits and find the tag it was encoded with
Function getTag:ULong(x:Double) Inline
    'GCSuspend()
    Local ulong_ptr:ULong Ptr = Varptr x
    Local result:ULong = ulong_ptr[0] Shr 48
    'GCResume()
    Return result
End Function

''' NaN-boxing specific functions:
'''    box(t,i): returns a New NaN-boxed Double with tag t And ordinal i
Function box:Double(tag:ULong, i:ULong) Inline
    Local temp:Double
    'GCSuspend()
    Local u_longptr:ULong Ptr = Varptr temp
    u_longptr[0] = tag Shl 48 | i
    'GCResume()
    Return temp
End Function

''' ord(x): returns the ordinal of the NaN-boxed Double x
''' not representative of *actual* value in base 10
Function ord:ULong(x:Double) Inline
    Local temp:ULong
    'GCSuspend()
    Local u_longptr:ULong Ptr = Varptr x
    temp = u_longptr[0] & $ffffffffffff:ULong
    'GCResume()
    Return temp
End Function

' Does nothing, but could be extended to check for NaN
Function num:Double(n:Double)
    Return n
End Function

' Returns nonzero if x equals y
Function equ:ULong(x:Double, y:Double)
    Local temp:ULong
    'GCSuspend()
    Local u_longptr_x:ULong Ptr = Varptr x
    Local u_longptr_y:ULong Ptr = Varptr y
    temp = (u_longptr_x[0] = u_longptr_y[0])
    'GCResume()
    Return temp
EndFunction

' interning of atom names (Lisp symbols), returns a unique NaN-boxed ATOM
Function atom:Double(s:String)
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
End Function

' construct pair (x . y) returns a NaN-boxed CONS_TAG
Function cons:Double(x:Double, y:Double)
    sp :- 1
    cell[sp] = x
    sp :- 1
    cell[sp] = y
    Return box(CONS_TAG, sp)
End Function

' return the car of the pair or ERR if not a pair
Function car:Double(p:Double) Inline
    If lispNot(p) Return nil_val ' modified

        If getTag(p) & ~(CONS_TAG ~ CLOS_TAG) = CONS_TAG
            Return cell[ord(p) + 1]
        End If
        Return err_val
End Function

' return the cdr of the pair or ERR if not a pair
Function cdr:Double(p:Double) Inline
    If lispNot(p) Return nil_val ' modified

        If getTag(p) & ~(CONS_TAG ~ CLOS_TAG) = CONS_TAG
            Return cell[ord(p)]
        End If
        Return err_val
End Function

' NOTE currently it is only safe To use atoms - Not collections
' You CAN'T modify the value If bound To a list TODO - FIX
Function f_setq:Double(t:Double, e:Double)
    Local v:Double = car(t)
    Local x:Double = eval(second(t), e)

    While getTag(t) = CONS_TAG And Not equ(v, car(car(e)))
        e = cdr(e)
    wend

    If getTag(e) = CONS_TAG
        cell[ord(car(e))] = x
        Return x
    End if

    Return err_val
End Function

' Replace the CAR of a list
Function f_rplaca:Double(t:Double, e:Double)
    Local tt:Double = evlis(t, e)
    Local p:Double = car(tt)
    Local ret:Double = err_val

    If getTag(p) = CONS_TAG
        ret = second(tt)
        cell[ord(p) + 1] = ret
    End If

    Return ret
End Function

' Replace the CDR of a list
Function f_rplacd:Double(t:Double, e:Double)
    Local tt:Double = evlis(t, e)
    Local p:Double = car(tt)
    Local ret:Double = err_val

    If getTag(p) = CONS_TAG
        ret = second(tt)
        cell[ord(p)] = ret
    End If

    Return ret
End Function

' Since car(cdr(...)) is used so much, let's just define 'second'
Function second:Double(p:Double)
    Return car(cdr(p))
End Function

Function third:Double(p:Double)
    Return car(cdr(cdr(p)))
End Function

Function fourth:Double(p:Double)
    Return car(cdr(cdr(cdr(p))))
End Function

' construct a pair to add to environment e, returns the list ((v . x) . e)
Function pair:Double(v:Double, x:Double, e:Double)
    Return cons(cons(v, x), e)
End Function

' construct a closure, returns a NaN-boxed CLOS_TAG
Function closure:Double(v:Double, x:Double, e:Double)
    Local env:Double
    If equ(e, env_val) Then env = nil_val Else env = e
    Return box(CLOS_TAG, ord(pair(v, x, env)))
End Function

' look up a symbol in an environment, return its value or err if not found
Function assoc:Double(v:Double, e:Double)
    While getTag(e) = CONS_TAG And (Not equ(v, car(car(e))))
        e = cdr(e)
    Wend
    If getTag(e) = CONS_TAG Then Return cdr(car(e))
    Return err_val
End Function

' lispNot(x) is nonzero if x is the lisp () empty list
Function lispNot:ULong(x:Double)
    Return getTag(x) = NIL_TAG
End Function

Function let:Ulong(x:Double)
    Return getTag(x) <> NIL_TAG And getTag(cdr(x)) <> NIL_TAG
End Function

Function evlis:Double(t:Double, e:Double)
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
End Function

' --------------------------------------------------------------------
' these f_ prefixed functions get called in the Function lookup table
' t is a parameters list, e is the Global environment
' --------------------------------------------------------------------

Function f_assoc:Double(t:Double, e:Double)
    Local tt:Double = evlis(t, e)
    Return assoc(car(tt), second(tt))
End Function

Function f_atoms_family:Double(t:Double, e:Double)
    Return e
End Function

Function f_eval:Double(t:Double, e:Double)
    Return eval(car(evlis(t, e)), e)
End Function

Function f_quote:Double(t:Double, _:Double)
    Return car(t)
End Function

Function f_cons:Double(t:Double, e:Double)
    t = evlis(t, e)
    Return cons(car(t), second(t))
End Function

Function f_list:Double(t:Double, e:Double)
    Return evlis(t, e)
End Function

Function f_car:Double(t:Double, e:Double) Inline
    Return car(car(evlis(t, e)))
End Function

Function f_cdr:Double(t:Double, e:Double) Inline
    Return cdr(car(evlis(t, e)))
End Function

Function f_add:Double(t:Double, e:Double)
    t = evlis(t, e)
    If getTag(t) = NIL_TAG Then Return num(0)
    Local n:Double = 0
    While Not lispNot(t)
        n :+ car(t)
        t = cdr(t)
    Wend
    Return num(n)
End Function

Function f_sub:Double(t:Double, e:Double)
    t = evlis(t, e)
    If getTag(t) = NIL_TAG Then Return err_val
    Local n:Double = car(t)
    If getTag(cdr(t)) = NIL_TAG Then Return num(-n)
    t = cdr(t)
    While Not lispNot(t)
        n :- car(t)
        t = cdr(t)
    Wend
    Return num(n)
End Function

Function f_mul:Double(t:Double, e:Double)
    t = evlis(t, e)
    If getTag(t) = NIL_TAG Then Return num(1)
    Local n:Double = car(t)
    t = cdr(t)
    While Not lispNot(t)
        n :* car(t)
        t = cdr(t)
    Wend
    Return num(n)
End Function

Function f_div:Double(t:Double, e:Double)
    t = evlis(t, e)
    Local n:Double = car(t)
    If getTag(t) = NIL_TAG Then Return err_val
    If getTag(cdr(t)) = NIL_TAG Then Return num(1.0 / n)
    t = cdr(t)
    While Not lispNot(t)
        n :/ car(t)
        t = cdr(t)
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
    If car(t) < second(t) Then Return tru_val
    Return nil_val
End Function

Function f_zerop:Double(t:Double, e:Double)
    t = evlis(t, e)
    If car(t) = 0 Then Return tru_val
    Return nil_val
End Function

Function f_eq:Double(t:Double, e:Double)
    t = evlis(t, e)
    If equ(car(t), second(t)) Then Return tru_val
    Return nil_val
End Function

' TODO Double check
Function f_eqNum:Double(t:Double, e:Double)
    t = evlis(t, e)
    If car(t) = second(t) Then Return tru_val
    Return nil_val
End Function

Function f_not_eq:Double(t:Double, e:Double)
    t = evlis(t, e)
    If car(t) <> second(t) Then Return tru_val
    Return nil_val
End Function

Function f_lteq:Double(t:Double, e:Double)
    t = evlis(t, e)
    If car(t) <= second(t) Then Return tru_val
    Return nil_val
End Function

Function f_gt:Double(t:Double, e:Double)
    t = evlis(t, e)
    If car(t) > second(t) Then Return tru_val
    Return nil_val
End Function

Function f_gteq:Double(t:Double, e:Double)
    t = evlis(t, e)
    If car(t) >= second(t) Then Return tru_val
    Return nil_val
End Function

Function f_not:Double(t:Double, e:Double)
    If lispNot(car(evlis(t, e))) Then Return tru_val
    Return nil_val
End Function

Function f_or:Double(t:Double, e:Double)
    Local x:Double = nil_val
    While getTag(t) <> NIL_TAG
        x = eval(car(t), e)
        If Not lispNot(x) Then Return x
        t = cdr(t)
    Wend
    Return x
End Function

Function f_and:Double(t:Double, e:Double)
    Local x:Double = nil_val
    While getTag(t) <> NIL_TAG
        x = eval(car(t), e)
        If lispNot(x) Then Exit
        t = cdr(t)
    Wend
    Return x
End Function

Function f_cond:Double(t:Double, e:Double) ' TODO handle no matches
    While getTag(t) <> NIL_TAG And lispNot(eval(car(car(t)), e))
        t = cdr(t)
    Wend
    If getTag(t) = NIL_TAG Then Return nil_val ' ala Common Lisp
    Return f_progn(cdr(car(t)), e)
End Function

' Deviation from TinyLisp, Lisp Machine style long else
Function f_if:Double(t:Double, e:Double)
    Local pred:Double = eval(car(t), e)
    Local rest_exps:Double = cdr(t)
    Local true_exp:Double = car(rest_exps)
    Local false_exps:Double = cdr(rest_exps)
    If Not lispNot(pred) ' anything but NIL/'()
        Return eval(true_exp, e)
    End If
    Return f_progn(false_exps, e) 'eval else expressions
End Function

' List ex: ((foo (bar 21) (baz 34)) <expr> ... <expr n>)
Function f_let_star:Double(t:Double, e:Double)
    Local binding_list:Double = car(t)
    Local sexps:Double = cdr(t)

    While Not LispNot(binding_list)
        Local current_binding:Double = car(binding_list)
        ' If symbol, bind symbol To nil And add.
        If getTag(current_binding) = ATOM_TAG
            e = pair(current_binding, nil_val, e)
        Else ' use the car And cadr of the pair
            e = pair(car(current_binding), eval(second(current_binding), e), e)
        End If

        binding_list = cdr(binding_list)
    Wend
    Return f_progn(sexps, e)
End Function

' List ex: ((foo (bar 21) (baz 34)) <expr> ... <expr n>)
Function f_let:Double(t:Double, e:Double)
    Local binding_list:Double = car(t)
    Local sexps:Double = cdr(t)
    Local cons_list:Double = nil_val
    While Not LispNot(binding_list)
        Local current_binding:Double = car(binding_list)
        ' If symbol, bind symbol To nil And add.
        If getTag(current_binding) = ATOM_TAG
            cons_list = cons(cons(current_binding, nil_val), cons_list)
        Else ' use the car And cadr of the pair
            cons_list = cons(cons(car(current_binding),
            eval(second(current_binding), e)), cons_list)
        End If

        binding_list = cdr(binding_list)
    Wend

    While Not lispNot(cons_list)
        Local cur_pair:Double = car(cons_list)
        e = pair(car(cur_pair), cdr(cur_pair), e)
        cons_list = cdr(cons_list)
    Wend

    Return f_progn(sexps, e)
End Function

' Currently dangerous, can blow the stack (as paper implements it)
' TODO make (letrec* ((f (...)) (f2 (...))) ... ) version
Function f_letreca:Double(t:Double, e:Double)
    While let(t)
        e = pair(car(car(t)), err_val, e)
        cell[sp + 2] = eval(car(cdr(car(t))), e)
        t = cdr(t)
    Wend
    Return eval(car(t), e)
End Function

' Prints all items provided
Function f_print:Double(t:Double, e:Double)
    While Not lispNot(t)
        lispPrint(eval(car(t), e))
        t = cdr(t)
    Wend
    Return nil_val
End Function

Function f_terpri:Double(t:Double, e:Double)
    Print "" ' just a New line
    Return nil_val
End Function

Function f_lambda:Double(t:Double, e:Double)
    ' we depart from TinyLisp once more, adding implicit progn
    Return closure(car(t), cons(atom("progn"), cdr(t)), e)
End Function

Function f_progn:Double(t:Double, e:Double)
    Local result:Double = nil_val
    While Not lispNot(t)
        result = eval(car(t), e)
        t = cdr(t)
    Wend
    Return result
End Function

Function f_prog1:Double(t:Double, e:Double)
    Local result:Double = nil_val
    Local temp:Double = nil_val
    Local counter:Int = 0
    While Not lispNot(t)
        temp = eval(car(t), e)
        If counter = 0 Then result = temp
        t = cdr(t)
        counter :+ 1
    Wend
    Return result
End Function

Function f_prog2:Double(t:Double, e:Double)
    Local result:Double = nil_val
    Local temp:Double = nil_val
    Local counter:Int = 0
    While Not lispNot(t)
        temp = eval(car(t), e)
        If counter = 1 Then result = temp
        t = cdr(t)
        counter :+ 1
    Wend
    Return result
End Function

Function f_define:Double(t:Double, e:Double)
    If getTag(car(t)) = CONS_TAG ' Function syntatic sugar
        Local symArgParms:Double = car(t)
        Local sym:Double = car(symArgParms)
        Local parms:Double = cdr(symArgParms)
        Local exps:Double = cdr(t)
        Local lambdaList:Double = cons(parms, exps)
        env_val = pair(sym, eval(f_lambda(lambdaList, e), e), env_val)
        Return sym
    Else
        env_val = pair(car(t), eval(second(t), e), env_val)
        Return car(t)
    EndIf

End Function

' t is (name (parms ...) statements ... )
Function f_defun:Double(t:Double, e:Double)
    Return f_define(cons(cons(car(t), second(t)), cdr(cdr(t))), e)
End Function

Function f_quit:Double(t:Double, e:Double)
    Return quit_val
End Function

Function f_time:Double(t:Double, e:Double)
    Local start:Int = MilliSecs()
    Local ret:Double = f_eval(t, e)
    Print "MilliSecs: " + (MilliSecs() - start)
    Return ret
End Function

Function f_Graphics:double(t:Double, e:Double)
    Local parsed:Double = evlis(t, e) ' meh optimize in future
    AppTitle = "B-LISP Visual Environment"
    Graphics Int(car(parsed)), Int(second(parsed))
    Return nil_val
End Function

Function f_Cls:Double(t:Double, e:Double)
    Cls
    Return nil_val
End Function

Function f_Flip:Double(t:Double, e:Double)
    Flip
    Return nil_val
End Function

Function f_set_color:Double(t:Double, e:Double)
    Local parsed:Double = evlis(t, e) ' meh optimize in future
    SetColor Int(car(parsed)), Int(second(parsed)), Int(third(parsed))
    Return nil_val
End Function

Function f_draw_line:Double(t:Double, e:Double)
    Local parsed:Double = evlis(t, e) ' meh optimize in future
    DrawLine car(parsed), second(parsed), third(parsed), fourth(parsed)
    Return nil_val
End function

Function f_draw_rect:Double(t:Double, e:Double)
    Local parsed:Double = evlis(t, e) ' meh optimize in future
    DrawRect car(parsed), second(parsed), third(parsed), fourth(parsed)
    Return nil_val
End Function

Function f_unit_test:Double(t:Double, e:Double)
    Local parsed:Double = evlis(t, e) ' meh optimize in future
    ' pray For BlitzMax
    ' (lambda (i) (lambda (x) (+ x i)))
    Local inpt:Double = cons(atom("lambda"), cons(cons(atom("i"), nil_val), cons(cons(atom("lambda"), cons(cons(atom("x"), nil_val), cons(cons(atom("+"), cons(atom("x"), cons(atom("i"), nil_val))), nil_val))), nil_val)))
    Local answer:Double = num(30)
    
    Local generator:Double = eval(inpt, e)
    Local clos:Double = eval(cons(generator, cons(num(10.0), nil_Val)), e)
    Local ret:Double = eval(cons(clos, cons(num(20.0), nil_Val)), e)
    Local passed:Double = f_eqNum(cons(ret, cons(answer, nil_val)), e)
    Return passed
End function

Function f_dummy:Double(t:Double, e:Double)
    Print "Not yet implimented"
    Return nil_val
End Function

Function niy(killLisp:Int = 0, msg:String = "Unhandled situation")
    Print "NIY: " + msg
    If killLisp
        Print "You are returning To the BlitzMax shell, press q hit Enter."
        Throw "Shit my pants - damn."
    End if
End Function
    
' table of Lisp primitives, each has a name s And a Function pointer f
Type fnPointer
    Field s:String
    Field f:Double(t:Double, e:Double)
    Method New (s:String, f:Double(t:Double, e:Double))
        Self.s = s
        Self.f = f
    End Method
End Type


' --------------------------------------------------------------------
' This is a table of primitive functions called by symbol lookup
' --------------------------------------------------------------------

' Given a symbol Return the Function it represents
Global prim:fnPointer[] = [ ..
New fnPointer("assoc", f_assoc),
New fnPointer("atoms-family", f_atoms_family),
New fnPointer("eval"        , f_eval),
New fnPointer("quote"       , f_quote),
New fnPointer("cons"        , f_cons),
New fnPointer("list"        , f_list),
New fnPointer("car"         , f_car),
New fnPointer("cdr"         , f_cdr),
' Mutating functions
New fnPointer("setq"        , f_setq),
New fnPointer("rplaca"       , f_rplaca),
New fnPointer("rplacd"       , f_rplacd),
' ----
New fnPointer("+"           , f_add),
New fnPointer("-"           , f_sub),
New fnPointer("*"           , f_mul),
New fnPointer("/"           , f_div),
New fnPointer("int"         , f_int),
New fnPointer("<"           , f_lt),    ' TODO variadic
New fnPointer("<="          , f_lteq),  ' TODO variadic
New fnPointer(">"           , f_gt),    ' TODO variadic
New fnPointer(">="          , f_gteq),  ' TODO variadic
New fnPointer("/="          , f_not_eq),' TODO variadic
New fnPointer("eq?"         , f_eq),    ' TODO variadic
New fnPointer("="           , f_eqNum), ' TODO variadic FIX REGEX TABLE
New fnPointer("zerop"       , f_zerop), ' maybe variadic
New fnPointer("or"          , f_or),
New fnPointer("and"         , f_and),
New fnPointer("not"         , f_not),
New fnPointer("cond"        , f_cond),
New fnPointer("if"          , f_if),
New fnPointer("let"         , f_let),
New fnPointer("let*"        , f_let_star),
New fnPointer("letrec*"     , f_letreca),
New fnPointer("lambda"      , f_lambda),
New fnPointer("define"      , f_define),
New fnPointer("defun"       , f_defun),
New fnPointer("progn"       , f_progn),
New fnPointer("prog1"       , f_prog1),
New fnPointer("prog2"       , f_prog2),
New fnPointer("print"       , f_print),
New fnPointer("terpri"      , f_terpri),
New fnPointer("time"        , f_time),
New fnPointer("quit"        , f_quit),
' stupid Graphics fun
New fnPointer("graphics"    , f_graphics),
New fnPointer("cls"         , f_cls),
New fnPointer("flip"        , f_flip),
New fnPointer("set-color"   , f_set_color),
New fnPointer("draw-line"   , f_draw_line),
New fnPointer("draw-rect"   , f_draw_rect),
New fnPointer("unit-test"   , f_unit_test)]

' create environment by extending e with the variables v bount to values t
Function bind:Double(v:Double, t:Double, e:Double)
    Select getTag(v)
        Case NIL_TAG Return e
        Case CONS_TAG Return bind(cdr(v), cdr(t), pair(car(v), car(t), e))
        Default Return pair(v, t, e)
    End Select
End Function

Function reduce:Double(f:Double, t:Double, e:Double)
    Local en:Double = cdr(f)
    If lispNot(cdr(f)) Then en = env_val
    Return eval(cdr(car(f)), bind(car(car(f)), evlis(t, e), en))
End Function

Function Apply:Double(f:Double, t:Double, e:Double)
    Select getTag(f)
        Case PRIM_TAG Return prim[ord(f)].f(t, e) ' look up in prim array
        Case CLOS_TAG Return reduce(f, t, e) '
        Default Return err_val
    End Select
End Function

Function eval:Double(x:Double, e:Double)
    Select getTag(x)
        Case ATOM_TAG Return assoc(x, e)
        Case CONS_TAG Return Apply(eval(car(x), e), cdr(x), e)
        Default Return x
    End Select
End Function

Function printlist(t:Double)
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
End Function

Function lispPrint(x:Double)
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
End Function

Function apiPrintlist(t:Double)
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
End Function

Function apiPrint(x:Double)
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
            niy(1, "apiPrint(): CLOSURE<" + ULong(ord(x)) + ">")
        Default
            Local F:TFormatter = New TFormatter.Create("%.10f") ' test
            prin "num(" + F.arg(x).format() + ")"
    End Select
End Function


' Yeah, this isn't going to stick around Forever. :D
' Function gc()
'     sp = ord(env_val)
' End Function

Function gc()
    sp = ord(env_val)

    For Local i:Double = EachIn cell
        If getTag(i) = ATOM_TAG And ord(i) > hp Then hp = ord(i)
    Next

    GCSuspend()
    Local A:Byte Ptr = cell
    hp :+ String.fromCString(A + hp).length + 1
    GCResume()
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

