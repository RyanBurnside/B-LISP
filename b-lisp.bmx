SuperStrict

' This module follows much of the TinyLisp (99 lines of C) implementation
' https://github.com/Robert-van-Engelen/tinylisp
' Also it takes some inspriation from the 1000 line extension
' https://github.com/Robert-van-Engelen/lisp

Import brl.retro
Import text.format
Import Text.RegEx

' table of Lisp primitives, each has a name s And a Function pointer f
Type fnPointer
    Field s:String
    Field f:Double(b:Blisp, t:Double, e:Double)
    Field parent:Blisp
    Method New (parent:Blisp, s:String, f:Double(b:Blisp, t:Double, e:Double))
        Self.s = s
        Self.f = f
        Self.parent = parent
    End Method
End Type


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
        Print "Seeding the table of primitives - these should be seperated later..."
        ' fns is not set
        Self.prim = [ ..
        New fnPointer(Self, "assoc", f_assoc),
        New fnPointer(Self, "atoms-family", f_atoms_family),
        New fnPointer(Self, "eval"        , f_eval),
        New fnPointer(Self, "quote"       , f_quote),
        New fnPointer(Self, "cons"        , f_cons),
        New fnPointer(Self, "list"        , f_list),
        New fnPointer(Self, "car"         , f_car),
        New fnPointer(Self, "cdr"         , f_cdr),
        ' Mutating functions - Not allowed To change Data length (Self, currnetly)
        New fnPointer(Self, "setq"        , f_setq),
        New fnPointer(Self, "rplaca"      , f_rplaca),
        New fnPointer(Self, "rplacd"      , f_rplacd),
        ' ----
        New fnPointer(Self, "+"           , f_add),
        New fnPointer(Self, "-"           , f_sub),
        New fnPointer(Self, "*"           , f_mul),
        New fnPointer(Self, "/"           , f_div),
        New fnPointer(Self, "int"         , f_int),
        New fnPointer(Self, "<"           , f_lt),    ' TODO variadic
        New fnPointer(Self, "<="          , f_lteq),  ' TODO variadic
        New fnPointer(Self, ">"           , f_gt),    ' TODO variadic
        New fnPointer(Self, ">="          , f_gteq),  ' TODO variadic
        New fnPointer(Self, "/="          , f_not_eq),' TODO variadic
        New fnPointer(Self, "eq?"         , f_eq),    ' TODO variadic
        New fnPointer(Self, "="           , f_eqNum), ' TODO variadic FIX REGEX TABLE
        New fnPointer(Self, "zerop"       , f_zerop), ' maybe variadic
        New fnPointer(Self, "or"          , f_or),
        New fnPointer(Self, "and"         , f_and),
        New fnPointer(Self, "not"         , f_not),
        New fnPointer(Self, "cond"        , f_cond),
        New fnPointer(Self, "if"          , f_if),
        New fnPointer(Self, "let"         , f_let),
        New fnPointer(Self, "let*"        , f_let_star),
        New fnPointer(Self, "letrec*"     , f_letreca),
        New fnPointer(Self, "lambda"      , f_lambda),
        New fnPointer(Self, "while"       , f_while),
        New fnPointer(Self, "define"      , f_define),
        New fnPointer(Self, "defun"       , f_defun),
        New fnPointer(Self, "progn"       , f_progn),
        New fnPointer(Self, "prog1"       , f_prog1),
        New fnPointer(Self, "prog2"       , f_prog2),
        New fnPointer(Self, "print"       , f_print),
        New fnPointer(Self, "random"      , f_random),
        New fnPointer(Self, "terpri"      , f_terpri),
        New fnPointer(Self, "time"        , f_time),
        New fnPointer(Self, "quit"        , f_quit),
        ' stupid Graphics fun
        New fnPointer(Self, "graphics"    , f_graphics),
        New fnPointer(Self, "cls"         , f_cls),
        New fnPointer(Self, "flip"        , f_flip),
        New fnPointer(Self, "set-color"   , f_set_color),
        New fnPointer(Self, "draw-line"   , f_draw_line),
        New fnPointer(Self, "draw-rect"   , f_draw_rect),
        New fnPointer(Self, "unit-test"   , f_unit_test)]
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

    ' NOTE currently it is only safe To use atoms - Not collections
    ' You CAN'T modify the value If bound To a list TODO - FIX
    Function f_setq:Double(b:Blisp, t:Double, e:Double)
        Local v:Double = b.car(t)
        Local x:Double = b.eval(b.second(t), e)

        While b.getTag(t) = b.CONS_TAG And Not b.equ(v, b.car(b.car(e)))
            e = b.cdr(e)
        Wend

        If b.getTag(e) = b.CONS_TAG
            b.cell[b.ord(b.car(e))] = x
            Return x
        End If

        Return b.err_val
    End Function

    ' Replace the CAR of a list
    Function f_rplaca:Double(b:Blisp, t:Double, e:Double)
        Local tt:Double = b.evlis(t, e)
        Local p:Double = b.car(tt)
        Local ret:Double = b.err_val

        If b.getTag(p) = b.CONS_TAG
            ret = b.second(tt)
            b.cell[b.ord(p) + 1] = ret
        End If

        Return ret
    End Function

    ' Replace the CDR of a list
    Function f_rplacd:Double(b:Blisp, t:Double, e:Double)
        Local tt:Double = b.evlis(t, e)
        Local p:Double = b.car(tt)
        Local ret:Double = b.err_val

        If b.getTag(p) = b.CONS_TAG
            ret = b.second(tt)
            b.cell[b.ord(p)] = ret
        End If

        Return ret
    End Function

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

    Function f_assoc:Double(b:Blisp, t:Double, e:Double)
        Local tt:Double = b.evlis(t, e)
        Return b.lisp_assoc(b.car(tt), b.second(tt))
    End Function

    Function f_atoms_family:Double(b:Blisp, t:Double, e:Double)
        Return e
    End Function

    Function f_eval:Double(b:Blisp, t:Double, e:Double)
        Return b.eval(b.car(b.evlis(t, e)), e)
    End Function

    Function f_quote:Double(b:Blisp, t:Double, _:Double)
        Return b.car(t)
    End Function

    Function f_cons:Double(b:Blisp, t:Double, e:Double)
        t = b.evlis(t, e)
        Return b.cons(b.car(t), b.second(t))
    End Function

    Function f_list:Double(b:Blisp, t:Double, e:Double)
        Return b.evlis(t, e)
    End Function

    Function f_car:Double(b:Blisp, t:Double, e:Double) 
        Return b.car(b.car(b.evlis(t, e)))
    End Function

    Function f_cdr:Double(b:Blisp, t:Double, e:Double) 
        Return b.cdr(b.car(b.evlis(t, e)))
    End Function

    Function f_add:Double(b:Blisp, t:Double, e:Double)
        t = b.evlis(t, e)
        If b.getTag(t) = b.NIL_TAG Then Return b.num(0)
        Local n:Double = 0
        While Not b.lispNot(t)
            n :+ b.car(t)
            t = b.cdr(t)
        Wend
        Return b.num(n)
    End Function

    Function f_sub:Double(b:Blisp, t:Double, e:Double)
        t = b.evlis(t, e)
        If b.getTag(t) = b.NIL_TAG Then Return b.err_val
        Local n:Double = b.car(t)
        If b.getTag(b.cdr(t)) = b.NIL_TAG Then Return b.num(-n)
        t = b.cdr(t)
        While Not b.lispNot(t)
            n :- b.car(t)
            t = b.cdr(t)
        Wend
        Return b.num(n)
    End Function

    Function f_mul:Double(b:Blisp, t:Double, e:Double)
        t = b.evlis(t, e)
        If b.getTag(t) = b.NIL_TAG Then Return b.num(1)
        Local n:Double = b.car(t)
        t = b.cdr(t)
        While Not b.lispNot(t)
            n :* b.car(t)
            t = b.cdr(t)
        Wend
        Return b.num(n)
    End Function

    Function f_div:Double(b:Blisp, t:Double, e:Double)
        t = b.evlis(t, e)
        Local n:Double = b.car(t)
        If b.getTag(t) = b.NIL_TAG Then Return b.err_val
        If b.getTag(b.cdr(t)) = b.NIL_TAG Then Return b.num(1.0 / n)
        t = b.cdr(t)
        While Not b.lispNot(t)
            n :/ b.car(t)
            t = b.cdr(t)
        Wend
        Return b.num(n)
    End Function

    Function f_int:Double(b:Blisp, t:Double, e:Double)
        Local n:Double = b.car(b.evlis(t,e))
        If n < 1e16 And n > 1e-16 Then Return Long(n)
        Return n
    End Function

    Function f_lt:Double(b:Blisp, t:Double, e:Double)
        t = b.evlis(t, e)
        If b.car(t) < b.second(t) Then Return b.tru_val
        Return b.nil_val
    End Function

    Function f_zerop:Double(b:Blisp, t:Double, e:Double)
        t = b.evlis(t, e)
        If b.car(t) = 0 Then Return b.tru_val
        Return b.nil_val
    End Function

    Function f_eq:Double(b:Blisp, t:Double, e:Double)
        t = b.evlis(t, e)
        If b.equ(b.car(t), b.second(t)) Then Return b.tru_val
        Return b.nil_val
    End Function

    ' TODO Double check
    Function f_eqNum:Double(b:Blisp, t:Double, e:Double)
        t = b.evlis(t, e)
        If b.car(t) = b.second(t) Then Return b.tru_val
        Return b.nil_val
    End Function

    Function f_not_eq:Double(b:Blisp, t:Double, e:Double)
        t = b.evlis(t, e)
        If b.car(t) <> b.second(t) Then Return b.tru_val
        Return b.nil_val
    End Function

    Function f_lteq:Double(b:Blisp, t:Double, e:Double)
        t = b.evlis(t, e)
        If b.car(t) <= b.second(t) Then Return b.tru_val
        Return b.nil_val
    End Function

    Function f_gt:Double(b:Blisp, t:Double, e:Double)
        t = b.evlis(t, e)
        If b.car(t) > b.second(t) Then Return b.tru_val
        Return b.nil_val
    End Function

    Function f_gteq:Double(b:Blisp, t:Double, e:Double)
        t = b.evlis(t, e)
        If b.car(t) >= b.second(t) Then Return b.tru_val
        Return b.nil_val
    End Function

    Function f_not:Double(b:Blisp, t:Double, e:Double)
        If b.lispNot(b.car(b.evlis(t, e))) Then Return b.tru_val
        Return b.nil_val
    End Function

    Function f_or:Double(b:Blisp, t:Double, e:Double)
        Local x:Double = b.nil_val
        While b.getTag(t) <> b.NIL_TAG
            x = b.eval(b.car(t), e)
            If Not b.lispNot(x) Then Return x
            t = b.cdr(t)
        Wend
        Return x
    End Function

    Function f_and:Double(b:Blisp, t:Double, e:Double)
        Local x:Double = b.nil_val
        While b.getTag(t) <> b.NIL_TAG
            x = b.eval(b.car(t), e)
            If b.lispNot(x) Then Exit
            t = b.cdr(t)
        Wend
        Return x
    End Function

    Function f_cond:Double(b:Blisp, t:Double, e:Double) ' TODO handle no matches
        While b.getTag(t) <> b.NIL_TAG And b.lispNot(b.eval(b.car(b.car(t)), e))
            t = b.cdr(t)
        Wend
        If b.getTag(t) = b.NIL_TAG Then Return b.nil_val ' ala Common Lisp
        Return Blisp.f_progn(b, b.cdr(b.car(t)), e)
    End Function

    ' Deviation from TinyLisp, Lisp Machine style long else
    Function f_if:Double(b:Blisp, t:Double, e:Double)
        Local pred:Double = b.eval(b.car(t), e)
        Local rest_exps:Double = b.cdr(t)
        Local true_exp:Double = b.car(rest_exps)
        Local false_exps:Double = b.cdr(rest_exps)
        If Not b.lispNot(pred) ' anything but NIL/'()
            Return b.eval(true_exp, e)
        End If
        Return Blisp.f_progn(b, false_exps, e) 'eval else expressions
    End Function

    ' List ex: ((foo (bar 21) (baz 34)) <expr> ... <expr n>)
    Function f_let_star:Double(b:Blisp, t:Double, e:Double)
        Local binding_list:Double = b.car(t)
        Local sexps:Double = b.cdr(t)

        While Not b.LispNot(binding_list)
            Local current_binding:Double = b.car(binding_list)
            ' If symbol, bind symbol To nil And add.
            If b.getTag(current_binding) = b.ATOM_TAG
                e = b.pair(current_binding, b.nil_val, e)
            Else ' use the car And cadr of the pair
                e = b.pair(b.car(current_binding), b.eval(b.second(current_binding), e), e)
            End If

            binding_list = b.cdr(binding_list)
        Wend
        Return f_progn(b, sexps, e)
    End Function

    ' List ex: ((foo (bar 21) (baz 34)) <expr> ... <expr n>)
    Function f_let:Double(b:BLisp, t:Double, e:Double)
        Local binding_list:Double = b.car(t)
        Local sexps:Double = b.cdr(t)
        Local cons_list:Double = b.nil_val
        While Not b.LispNot(binding_list)
            Local current_binding:Double = b.car(binding_list)
            ' If symbol, bind symbol To nil And add.
            If b.getTag(current_binding) = b.ATOM_TAG
                cons_list = b.cons(b.cons(current_binding, b.nil_val), cons_list)
            Else ' use the car And cadr of the pair
                cons_list = b.cons(b.cons(b.car(current_binding),
                b.eval(b.second(current_binding), e)), cons_list)
            End If

            binding_list = b.cdr(binding_list)
        Wend

        While Not b.lispNot(cons_list)
            Local cur_pair:Double = b.car(cons_list)
            e = b.pair(b.car(cur_pair), b.cdr(cur_pair), e)
            cons_list = b.cdr(cons_list)
        Wend

        Return f_progn(b, sexps, e)
    End Function

    ' Currently dangerous, can blow the stack (as paper implements it)
    ' TODO make (letrec* ((f (...)) (f2 (...))) ... ) version
    Function f_letreca:Double(b:Blisp, t:Double, e:Double)
        While b.let(t)
            e = b.pair(b.car(b.car(t)), b.err_val, e)
            b.cell[b.sp + 2] = b.eval(b.car(b.cdr(b.car(t))), e)
            t = b.cdr(t)
        Wend
        Return b.eval(b.car(t), e)
    End Function

    ' Prints all items provided
    Function f_print:Double(b:Blisp, t:Double, e:Double)
        While Not b.lispNot(t)
            b.lispPrint(b.eval(b.car(t), e))
            t = b.cdr(t)
        Wend
        Return b.nil_val
    End Function

    Function f_random:Double(b:Blisp, t:Double, e:Double)
        Return RndDouble()
    End Function

    Function f_terpri:Double(b:Blisp, t:Double, e:Double)
        Print "" ' just a New line
        Return b.nil_val
    End Function

    Function f_lambda:Double(b:Blisp, t:Double, e:Double)
        ' we depart from TinyLisp once more, adding implicit progn
        Return b.closure(b.car(t), b.cons(b.atom("progn"), b.cdr(t)), e)
    End Function

    Function f_while:Double(b:Blisp, t:Double, e:Double)
        ' (while <condition> <statements>)
        Local result:Double = b.nil_val
        While Not b.lispNot(b.eval(b.car(t), e))
            result = Blisp.f_progn(b, b.cdr(t), e)
        Wend
        Return result
    End Function
    
    Function f_progn:Double(b:Blisp, t:Double, e:Double)
        Local result:Double = b.nil_val
        While Not b.lispNot(t)
            result = b.eval(b.car(t), e)
            t = b.cdr(t)
        Wend
        Return result
    End Function

    Function f_prog1:Double(b:Blisp, t:Double, e:Double)
        Local result:Double = b.nil_val
        Local temp:Double = b.nil_val
        Local counter:Int = 0
        While Not b.lispNot(t)
            temp = b.eval(b.car(t), e)
            If counter = 0 Then result = temp
            t = b.cdr(t)
            counter :+ 1
        Wend
        Return result
    End Function

    Function f_prog2:Double(b:Blisp, t:Double, e:Double)
        Local result:Double = b.nil_val
        Local temp:Double = b.nil_val
        Local counter:Int = 0
        While Not b.lispNot(t)
            temp = b.eval(b.car(t), e)
            If counter = 1 Then result = temp
            t = b.cdr(t)
            counter :+ 1
        Wend
        Return result
    End Function

    Function f_define:Double(b:Blisp, t:Double, e:Double)
        If b.getTag(b.car(t)) = b.CONS_TAG ' Function syntatic sugar
            Local symArgParms:Double = b.car(t)
            Local sym:Double = b.car(symArgParms)
            Local parms:Double = b.cdr(symArgParms)
            Local exps:Double = b.cdr(t)
            Local lambdaList:Double = b.cons(parms, exps)
            b.env_val = b.pair(sym, b.eval(f_lambda(b, lambdaList, e), e), b.env_val)
            Return sym
        Else
            b.env_val = b.pair(b.car(t), b.eval(b.second(t), e), b.env_val)
            Return b.car(t)
        EndIf

    End Function

    ' t is (name (parms ...) statements ... )
    Function f_defun:Double(b:Blisp, t:Double, e:Double)
        Return f_define(b, b.cons(b.cons(b.car(t), b.second(t)), b.cdr(b.cdr(t))), e)
    End Function

    Function f_quit:Double(b:Blisp, t:Double, e:Double)
        Return b.quit_val
    End Function

    Function f_time:Double(b:Blisp, t:Double, e:Double)
        Local start:Int = MilliSecs()
        Local ret:Double = f_eval(b, t, e)
        Print "MilliSecs: " + (MilliSecs() - start)
        Return ret
    End Function

    Function f_Graphics:Double(b:Blisp, t:Double, e:Double)
        Local parsed:Double = b.evlis(t, e) ' meh optimize in future
        AppTitle = "B-LISP Visual Environment"
        Graphics Int(b.car(parsed)), Int(b.second(parsed))
        Return b.nil_val
    End Function

    Function f_Cls:Double(b:Blisp, t:Double, e:Double)
        Cls
        Return b.nil_val
    End Function

    Function f_Flip:Double(b:Blisp, t:Double, e:Double)
        Flip
        Return b.nil_val
    End Function

    Function f_set_color:Double(b:Blisp, t:Double, e:Double)
        Local parsed:Double = b.evlis(t, e) ' meh optimize in future
        SetColor Int(b.car(parsed)), Int(b.second(parsed)), Int(b.third(parsed))
        Return b.nil_val
    End Function

    Function f_draw_line:Double(b:Blisp, t:Double, e:Double)
        Local parsed:Double = b.evlis(t, e) ' meh optimize in future
        DrawLine b.car(parsed), b.second(parsed), b.third(parsed), b.fourth(parsed)
        Return b.nil_val
    End Function

    Function f_draw_rect:Double(b:Blisp, t:Double, e:Double)
        Local parsed:Double = b.evlis(t, e) ' meh optimize in future
        DrawRect b.car(parsed), b.second(parsed), b.third(parsed), b.fourth(parsed)
        Return b.nil_val
    End Function

    Function f_unit_test:Double(b:Blisp, t:Double, e:Double)
        Local testCounter:Int  = 0
        If b.test_lambda(t, e)
            Print "Lambda test passed"
            testCounter :+ 1
        End If
        Return testCounter
    End Function

    Function f_dummy:Double(b:Blisp, t:Double, e:Double)
        Print "Not yet implimented"
        Return b.nil_val
    End Function

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
        Local passed:Double = Blisp.f_eqNum(Self, cons(ret, cons(answer, nil_val)), e)
        Return passed
    End Method

    Method test_assoc:Double(t:Double, e:Double)
        Local Input:Double = cons(atom("assoc"), cons(cons(atom("quote"), cons(cons(cons(atom("a"), cons(atom("b"), nil_val)), cons(cons(atom("c"), cons(atom("d"), nil_val)), nil_val)), nil_val)), cons(cons(atom("quote"), cons(atom("a"), nil_val)), nil_val)))

    End Method

End Type
