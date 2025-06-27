
' This file contains the primitives used In the BLisp objects.
' They are not methods because we need Function pointers
' This also allows different loadouts on Blisp instances

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
    Return f_progn(b, b.cdr(b.car(t)), e)
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
    Return f_progn(b, false_exps, e) 'eval else expressions
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
        result = f_progn(b, b.cdr(t), e)
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

' Graphics section - maybe Graphics.bmx later ...

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

' Input polling
Function f_key_escapep:Double(b:Blisp, t:Double, e:Double)
    If KeyDown(KEY_ESCAPE) Return b.tru_val
    Return b.nil_val
End Function


' test/misc
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

' Some functions To make full loadouts For now, let's assume a Default loadout
' This gets called If no table is passed To a BLisp instance on New method
Function makeFullFnTable:fnPointer[](b:Blisp)
    Print "Calling makeFullFnTable..."
    Return [ ..
    New fnPointer(b, "assoc"       , f_assoc),
    New fnPointer(b, "atoms-family", f_atoms_family),
    New fnPointer(b, "eval"        , f_eval),
    New fnPointer(b, "quote"       , f_quote),
    New fnPointer(b, "cons"        , f_cons),
    New fnPointer(b, "list"        , f_list),
    New fnPointer(b, "car"         , f_car),
    New fnPointer(b, "cdr"         , f_cdr),
    ' Mutating functions - Not allowed To change Data length (b, currnetly)
    New fnPointer(b, "setq"        , f_setq),
    New fnPointer(b, "rplaca"      , f_rplaca),
    New fnPointer(b, "rplacd"      , f_rplacd),
    ' ----
    New fnPointer(b, "+"           , f_add),
    New fnPointer(b, "-"           , f_sub),
    New fnPointer(b, "*"           , f_mul),
    New fnPointer(b, "/"           , f_div),
    New fnPointer(b, "int"         , f_int),
    New fnPointer(b, "<"           , f_lt),    ' TODO variadic
    New fnPointer(b, "<="          , f_lteq),  ' TODO variadic
    New fnPointer(b, ">"           , f_gt),    ' TODO variadic
    New fnPointer(b, ">="          , f_gteq),  ' TODO variadic
    New fnPointer(b, "/="          , f_not_eq),' TODO variadic
    New fnPointer(b, "eq?"         , f_eq),    ' TODO variadic
    New fnPointer(b, "="           , f_eqNum), ' TODO variadic FIX REGEX TABLE
    New fnPointer(b, "zerop"       , f_zerop), ' maybe variadic
    New fnPointer(b, "or"          , f_or),
    New fnPointer(b, "and"         , f_and),
    New fnPointer(b, "not"         , f_not),
    New fnPointer(b, "cond"        , f_cond),
    New fnPointer(b, "if"          , f_if),
    New fnPointer(b, "let"         , f_let),
    New fnPointer(b, "let*"        , f_let_star),
    New fnPointer(b, "letrec*"     , f_letreca),
    New fnPointer(b, "lambda"      , f_lambda),
    New fnPointer(b, "while"       , f_while),
    New fnPointer(b, "define"      , f_define),
    New fnPointer(b, "defun"       , f_defun),
    New fnPointer(b, "progn"       , f_progn),
    New fnPointer(b, "prog1"       , f_prog1),
    New fnPointer(b, "prog2"       , f_prog2),
    New fnPointer(b, "print"       , f_print),
    New fnPointer(b, "random"      , f_random),
    New fnPointer(b, "terpri"      , f_terpri),
    New fnPointer(b, "time"        , f_time),
    New fnPointer(b, "quit"        , f_quit),
    
    ' stupid Graphics fun
    New fnPointer(b, "graphics"    , f_graphics),
    New fnPointer(b, "cls"         , f_cls),
    New fnPointer(b, "flip"        , f_flip),
    New fnPointer(b, "set-color"   , f_set_color),
    New fnPointer(b, "draw-line"   , f_draw_line),
    New fnPointer(b, "draw-rect"   , f_draw_rect),
    
    'Input handling
    New fnPointer(b, "key-escapep" , f_key_escapep),    
    New fnPointer(b, "unit-test"   , f_unit_test)]
End function
