SuperStrict

Import brl.retro
Import text.format
Import Text.RegEx

' For lexer
Enum TokenType
    SYMBOL,
    NUMBER,
    BACKQUOTE,
    SPLICE,
    COMMA,
    QUOTE,
    LPAREN,
    RPAREN,
    LDOT,
    SKIP, ' Handles whitespace
    ERROR,
    TEXT_EOF
End Enum

' NaN box constant "tags" For encoding into doubles
Const ATOM_TAG:ULong = $7ff8  'atom
Const PRIM_TAG:ULong = $7ff9  'primitive
Const CONS_TAG:ULong = $7ffa  'cons cell
Const CLOS_TAG:ULong = $7ffb  'closure
Const NIL_TAG:ULong  = $7ffc  'duh

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

' This module follows much of the TinyLisp (99 lines of C) implementation

' This stupid thing needs to exist because blitzmax
' mandates newlines with Print
Function prin(s:String)
    StandardIOStream.WriteString s
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
Function T:ULong(x:Double) Inline
    GCSuspend()
        Local ulong_ptr:ULong Ptr = Varptr x
        Local result:ULong = ulong_ptr[0] Shr 48
    GCResume()
    Return result
End Function

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
Function ord:ULong(x:Double) Inline
    Local temp:ULong
    GCSuspend()
        Local u_longptr:ULong Ptr = Varptr x
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
    If (hp > sp Shl 3)
        Print "Unrecoverable error: stack overflow"
        Local zero:Int = 0
        Local kill_me:Double = 1 / zero
    EndIf    
    Return box(CONS_TAG, sp)
End Function

' return the car of the pair or ERR if not a pair
Function car:Double(p:Double)
    If T(p) & ~(CONS_TAG ~ CLOS_TAG) = CONS_TAG
        Return cell[ord(p) + 1]
    End If
    Return err_val 
End Function

' return the cdr of the pair or ERR if not a pair
Function cdr:Double(p:Double)
    If T(p) & ~(CONS_TAG ~ CLOS_TAG) = CONS_TAG
        Return cell[ord(p)]
    End If
    Return err_val 
End Function

' construct a pair to add to environment e, returns the list ((v . x) . e)
Function pair:Double(v:Double, x:Double, e:Double)
    Return cons(cons(v, x), e)
End Function

' construct a closure, returns a NaN-boxed CLOS_TAG
Function closure:Double(v:Double, x:Double, e:Double)
    '  return box(CLOS,ord(pair(v,x,equ(e,env) ? nil : e))); }
    Local env:Double
    If equ(e, env_val) Then env = nil_val Else env = e
    Return box(CLOS_TAG, ord(pair(v, x, env)))
End Function

' look up a symbol in an environment, return its value or err if not found
Function assoc:Double(v:Double, e:Double)
    While T(e) = CONS_TAG And (Not equ(v, car(car(e))))
        e = cdr(e)
    Wend
    If T(e) = CONS_TAG Then Return cdr(car(e))
    Return err_val
End Function

' lispNot(x) is nonzero if x is the lisp () empty list
Function lispNot:ULong(x:Double)
    Return T(x) = NIL_TAG
End Function

' TODO REMOVE this trash once proper LET is formed
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
'   (+ n1 n2 ... nk)  sum of n1 to nk
'   (- n1 n2 ... nk)  n1 minus sum of n2 to nk
'   (* n1 n2 ... nk)  product of n1 to nk
'   (/ n1 n2 ... nk)  n1 divided by the product of n2 to nk
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
'   (if x y z ... z<n>)          if x is non-() then y else z exps
'   (let* (v1 x1)
'         (v2 x2)
'         ...
'         y)            sequentially binds each variable v1 to xi to evaluate y
'   (lambda v x)        construct a closure
'   (define v x)        define a named value globally

' the f_ series of functions get a parameter list (t or tt) and environment
' we can't use t because we defined T() to get the tag earlier. BlitzMax folks...
Function f_eval:Double(tt:Double, e:Double)
    Return eval(car(evlis(tt, e)), e)
End Function

Function f_quote:Double(tt:Double, _:Double)
    Return car(tt)
End Function

Function f_cons:Double(tt:Double, e:Double)
    tt = evlis(tt, e)
    Return cons(car(tt), car(cdr(tt)))
End Function

Function f_list:Double(tt:Double, e:Double)
    Return evlis(tt, e)
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
    tt = evlis(tt, e)
    If T(tt) = NIL_TAG Then Return err_val
    Local n:Double = car(tt)
    If T(cdr(tt)) = NIL_TAG Then Return num(-n)
    tt = cdr(tt)
    While Not lispNot(tt)
        n :- car(tt)
        tt = cdr(tt)
    Wend
    Return num(n)
End Function

Function f_mul:Double(tt:Double, e:Double)
    tt = evlis(tt, e)
    If T(tt) = NIL_TAG Then Return num(1)
    Local n:Double = car(tt)
    tt = cdr(tt)
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
    tt = cdr(tt)
    While Not lispNot(tt)
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
    If car(t) < car(cdr(t)) Then Return tru_val
    Return nil_val
End Function

Function f_eq:Double(t:Double, e:Double)
    t = evlis(t, e)
    If equ(car(t), car(cdr(t))) Then Return tru_val
    Return nil_val
End Function

Function f_not_eq:Double(t:Double, e:Double)
    t = evlis(t, e)
    If car(t) <> car(cdr(t)) Then Return tru_val
    Return nil_val
End Function

Function f_lteq:Double(t:Double, e:Double)
    t = evlis(t, e)
    If car(t) <= car(cdr(t)) Then Return tru_val
    Return nil_val
End Function

Function f_gt:Double(t:Double, e:Double)
    t = evlis(t, e)
    If car(t) > car(cdr(t)) Then Return tru_val
    Return nil_val
End Function

Function f_gteq:Double(t:Double, e:Double)
    t = evlis(t, e)
    If car(t) >= car(cdr(t)) Then Return tru_val
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

Function f_and:Double(tt:Double, e:Double)
    Local x:Double = nil_val
    While T(tt) <> NIL_TAG
        x = eval(car(tt), e)
        If lispNot(x) Then Exit
        tt = cdr(tt)
    Wend
    Return x
End Function

Function f_cond:Double(tt:Double, e:Double)
    While T(tt) <> NIL_TAG And lispNot(eval(car(car(tt)), e))
        tt = cdr(tt)
    Wend
    Return eval(car(cdr(car(tt))), e)
End Function

' Deviation from TinyLisp, Emacs' style long else
Function f_if:Double(tt:Double, e:Double)
    Local pred:Double = eval(car(tt), e)
    Local rest_exps:Double = cdr(tt)
    Local true_exp:Double = car(rest_exps)
    Local false_exps:Double = cdr(rest_exps)
    If Not lispNot(pred) ' anything but NIL/'()
        Return eval(true_exp, e)
    End If
    Return f_progn(false_exps, e) 'eval else expressions
End Function

Function f_leta:Double(tt:Double, e:Double)
    While let(tt)
        e = pair(car(car(tt)), eval(car(cdr(car(tt))), e), e)
        tt = cdr(tt)
    Wend
    Return eval(car(tt), e)
End Function

' List ex: ((foo (bar 21) (baz 34)) <expr> ... <expr n>)
Function f_let_star:Double(tt:Double, e:Double)
    Local binding_list:double = car(tt)
    Local sexps:double = cdr(tt)
    
    While Not LispNot(binding_list)
        Local current_binding:Double = car(binding_list)
        ' If symbol, bind symbol To nil And add.
        If T(current_binding) = ATOM_TAG
            e = pair(current_binding, nil_val, e)
        Else ' use the car And cadr of the pair
            e = pair(car(current_binding), eval(car(cdr(current_binding)), e), e)
        End if
        
        binding_list = cdr(binding_list)
    Wend
    Print "Finished processing bindings"
    Return f_progn(sexps, e)
End Function

Function f_lambda:Double(tt:Double, e:Double)
    Return closure(car(tt), car(cdr(tt)), e)
End Function

Function f_progn:Double(tt:Double, e:Double)
    prin "f_progn got: "
    lispPrint(tt)
    Print ""
    Local result:Double = nil_val
    While Not lispNot(tt)
        result = eval(car(tt), e)
        tt = cdr(tt)
    Wend
    Return result
End Function

Function f_prog1:Double(tt:Double, e:Double)
    Local result:Double = nil_val
    Local counter:Int = 0
    While Not lispNot(tt)
        If counter = 0 Then result = eval(car(tt), e)
        tt = cdr(tt)
        counter :+ 1
    Wend
    Return result
End Function

Function f_prog2:Double(tt:Double, e:Double)
    Local result:Double = nil_val
    Local counter:Int = 0
    While Not lispNot(tt)
        If counter = 1 Then result = eval(car(tt), e)
        tt = cdr(tt)
        counter :+ 1
    Wend
    Return result
End Function

Function f_define:Double(tt:Double, e:Double)
    env_val = pair(car(tt), eval(car(cdr(tt)), e), env_val)
    Return car(tt)
End Function

Function f_quit:Double(tt:Double, e:Double)
    Return quit_val
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

' Given a symbol Return the Function it represents
Global prim:fnPointer[] = [ ..
New fnPointer("eval",     f_eval),
New fnPointer("quote",    f_quote),
New fnPointer("cons",     f_cons),
New fnPointer("list",     f_list),
New fnPointer("car",      f_car),
New fnPointer("cdr",      f_cdr),
New fnPointer("+",        f_add),
New fnPointer("-",        f_sub),
New fnPointer("*",        f_mul),
New fnPointer("/",        f_div),
New fnPointer("int",      f_int),
New fnPointer("<",        f_lt),    ' TODO variadic
New fnPointer("<=",       f_lteq),  ' TODO variadic
New fnPointer(">",        f_gt),    ' TODO variadic
New fnPointer(">=",       f_gteq),  ' TODO variadic
New fnPointer("/=",       f_not_eq),' TODO variadic
New fnPointer("eq?",      f_eq),    ' TODO variadic
New fnPointer("or",       f_or),
New fnPointer("and",      f_and),
New fnPointer("not",      f_not),
New fnPointer("cond",     f_cond),
New fnPointer("if",       f_if),
New fnPointer("bad-let*", f_leta),
New fnPointer("let",      f_leta),
New fnPointer("let*",     f_let_star),
New fnPointer("lambda",   f_lambda),
New fnPointer("define",   f_define),
New fnPointer("progn",    f_progn),
New fnPointer("prog1",    f_prog1),
New fnPointer("prog2",    f_prog2),
New fnPointer("quit",     f_quit)]

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

Function printlist(tt:Double)
    prin "("
    Repeat
        lispPrint car(tt)
        tt = cdr(tt)
        If T(tt) = NIL_TAG Then Exit
        If T(tt) <> CONS_TAG
            prin " . "
            lispPrint(tt)
            Exit
        End If
        prin " "
    Forever    
    prin ")"
End Function

Function lispPrint(x:Double)
    Select T(x)
    Case NIL_TAG prin "()"
    Case ATOM_TAG
        GCSuspend()
            Local A:Byte Ptr = cell
            prin "".FromCString(A+ord(x))
        GCResume()
    Case PRIM_TAG prin "<" + prim[ord(x)].s + ">"
    Case CONS_TAG printlist(x)
    Case CLOS_TAG prin "{" + ULong(ord(x)) + "}"
    Default
        Local F:TFormatter = New TFormatter.Create("%.10f") ' test
        prin F.arg(x).format()
    End Select
End Function

' Yeah, this isn't going to stick around Forever. :D
Function gc()
    sp = ord(env_val)
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

' this needs To be taken out later as Parser.bmx

Function stringToTokenType:TokenType(str:String)
    Select str
    Case "ATOM" Return TokenType.SYMBOL
    Case "SYMBOL" Return TokenType.SYMBOL
    Case "NUMBER" Return TokenType.NUMBER
    Case "BACKQUOTE" Return TokenType.BACKQUOTE
    Case "SPLICE" Return TokenType.SPLICE
    Case "COMMA" Return TokenType.COMMA
    Case "QUOTE" Return TokenType.QUOTE
    Case "LPAREN" Return TokenType.LPAREN
    Case "RPAREN" Return TokenType.RPAREN
    Case "LDOT" Return TokenType.LDOT
    Case "SKIP" Return TokenType.SKIP
    Case "ERROR" Return TokenType.ERROR
    Default Return TokenType.ERROR
    End Select
End Function

Type Token
    Field typ:TokenType
    Field value:String
    ' add line and column errors later (if one gives a shit)
    Global formatter:TFormatter = TFormatter.Create("<Token %s(%d), '%s' >")
    
    Method New(typ:TokenType, value:String)
        Self.typ = typ
        Self.value = value
    End Method
    
    Method ToString:String()
        formatter.Clear()
        formatter.Arg(typ.ToString()).Arg(Int(typ)).Arg(value)
        Return formatter.format()
    End Method

    Method Print()
        Print Self.ToString()
    End Method
End Type

' This is the regex table (to be transformed) for B-LISP
' Order matters, preference is given For earlier matches
Function makeRegexTableBlisp:String[][] ()
    ' Regex groups, order by most specific to least
    Return [["NUMBER",    "[-+]?\d+(\.\d*)?"],
            ["SYMBOL",    "[0-9A-Za-z!@#$%^&*\-+\\<>/?=]+"],
            ["BACKQUOTE", "`"],
            ["SPLICE",    ",@"],
            ["COMMA",     ","],
            ["QUOTE",     "'"],
            ["LPAREN",    "[\(]"],
            ["RPAREN",    "[\)]"],
            ["LDOT",      "\."],
            ["SKIP",      "[ \t\n]+"],
            ["ERROR",     "[^ \t\n]+"]]
End Function

' This turns a regex table into a single regex with named capture groups
Function RegexTableToRegex:TRegEx(regexTable:String[][])
    ' Accessory fn to transform the tokenSpecification sub arrays into regex group
    Function JoinTokenSpecs:String(ID:String, Regex:String)
        Global regPair:TFormatter = TFormatter.Create("(?P<%s>%s)")
        regPair.clear()
        regPair.Arg(ID).Arg(Regex)
        Return regPair.format()
    End Function
    
    Local temp:String[regexTable.Length]
    Local counter:Int = 0

    For Local i:String[] = EachIn regexTable
        temp[counter] = JoinTokenSpecs(i[0], i[1])
        counter :+ 1
    Next
    
    ' Multi-Part joined Regex String for token
    Return TRegEx.Create("|".join(temp))
End Function

' Used To turn regex matches into tokens For the Parser
Type Lexer
    ' Regex Class
    Field getToken:TRegEx
    
    ' Regex match class
    Field matcher:TRegExMatch
    Field regexTable:String[][]
    
    Method New(regexTable:String[][], text:String)
        Self.regexTable = regexTable 
        getToken = RegexTableToRegex(regexTable)
        matcher = getToken.Find(text)
    End Method
    
    Method NextToken:Token()
        Local captureType:TokenType
        Local matched:String
                        
        While matcher
            For Local n:String[] = EachIn regexTable
                Local captureName:String = n[0]
                matched:String = matcher.SubExpByName(captureName)
                captureType:TokenType = stringToTokenType(captureName)
                If matched <> "" And captureType <> TokenType.SKIP
                    matcher = getToken.Find()
                    ' Acts like yield, getToken has next match ready to go
                    Return New Token(captureType, matched)
                End If 
            Next
            matcher = getToken.Find()
        Wend
        Return New Token(TokenType.TEXT_EOF, "")
    End Method
End Type

' Parses tokens into low level BlitzMax operations on memory chunk
Type Parser
    Field lexer:Lexer
    Field currTok:Token
    
    Method New(lexer:Lexer)
        Self.lexer = lexer
        currTok = lexer.NextToken()
    End Method

    Method match:Byte(tt:TokenType)
        Return tt = currTok.typ
    End Method
    
    Method error()
        RuntimeError "Unexpected current token: " + currTok.ToString()
    End Method

    Method accept:Token()
        Local tempTok:Token = currTok
        currTok = lexer.NextToken()
        Return tempTok
    End Method
        
    Method parse:Double()
        If match(TokenType.NUMBER) Then Return parseNumber()
        If match(TokenType.SYMBOL) Then Return parseSymbol()
        If match(TokenType.LPAREN) 
            accept() ' Tossing left paren
            Return parseList()
        End If 
        If match(TokenType.QUOTE)
            accept() ' Toss quote
            Return parseQuote()    
        End If
        error()
    End Method
        
    Method parseNumber:Double()
        Local numTok:Token = accept()
        Return num(Double(numTok.value))
    End Method
    
    Method parseSymbol:Double()
        Local symTok:Token = accept()
        Return atom(symTok.value)
     End Method

    Method parseList:Double()
        Local expr:Double

        If match(TokenType.RPAREN)
            accept() ' Tosses right paren
            Return nil_val
        End If
        
        If match(TokenType.LDOT)
            accept() ' Tosses the LDOT
            expr:Double = parse()
            If match(TokenType.RPAREN) Then accept Else error
            Return expr
        End If
        
        expr:Double = parse()
        Return cons(expr, parseList())
    End Method

    Method parseQuote:Double()
        Return cons(atom("quote"), cons(parse(), nil_val))
    End Method
End Type

Function nan_main:Int()
    Print "Starting B-LISP"
    nil_val = box(NIL_TAG, 0)
    err_val = atom("ERR")
    tru_val = atom("#t")
    env_val = pair(tru_val, tru_val, nil_val)
    For Local i:ULong = 0 Until prim.Length
        env_val = pair(atom(prim[i].s), box(PRIM_TAG, i), env_val)
    Next
    
    Local val_a:Double = num(64)
    Local val_b:Double = num(36)    
    For Local op:String = EachIn ["*", "/", "+", "-"]
        Local expr:Double = cons(atom(op), cons(val_a, cons(val_b, nil_val)))
        Print "Doing the function: " + op + " with values: a: " + val_a + " b: " + val_b
        Print "Result is : " + eval(expr, env_val)
    Next

    Local subLis:Double = cons(box(PRIM_TAG, 6), cons(22, cons(32, nil_val)))
    Local sexp:Double = cons(box(PRIM_TAG, 5), cons(22, cons(subLis, cons(42, nil_val))))
    Print ""
    lispPrint(sexp)
    Print "~n"
    prin ">>>" + eval(sexp, env_val)
    Return 0
End Function

Function lexer_main()
    Local statements:String = """
        (* (+ FOO *BAR* +BAZ+) |foobar| BAZ 42 43.5)
        (/ 3 4)
        `(,SYM ,@(1 2 3 4))
        '(3 . 4)
"""
    Local lexer:Lexer = New Lexer(makeRegexTableBlisp(), statements)
    
    Local nextToken:Token = lexer.NextToken() 
    While nextToken.typ <> TokenType.TEXT_EOF 
        nextToken.Print()
        nextToken = lexer.NextToken()
    Wend
    nextToken.Print()
End Function

Function parser_main()
    Local ret:Double
    Print "Welcome to B-LISP."

    nil_val = box(NIL_TAG, 0)                 ' TODO make roots in real GC
    quit_val = box(NIL_TAG, 1)                ' TODO make roots in real GC
    
    err_val = atom("ERR")                     ' TODO make roots in real GC
    tru_val = atom("#t")                      ' TODO make roots in real GC
    env_val = pair(tru_val, tru_val, nil_val) ' TODO make roots in real GC

    For Local i:ULong = 0 Until prim.Length
        env_val = pair(atom(prim[i].s), box(PRIM_TAG, i), env_val)
    Next

    Repeat 
        ' READ
        Local cellsUsed:ULong = N - sp
        Local cellsRemaining:ULong = sp - Ceil(hp / 8.0)
        Local prompt:String = "B-LISP[" + cellsUsed + "/" + cellsRemaining + "]> "
        
        Local statements:String = Input(prompt).Trim()
        Local lexer:Lexer = New Lexer(makeRegexTableBlisp(), statements)
        Local parser:Parser = New Parser(lexer)
        Local sexp:Double = parser.parse()
        prin "Parsed Lisp Object: "
        lispPrint(sexp)
        Print ""

        ' EVAL
        Local start:Int = MilliSecs()
        ret = eval(sexp, env_val)
        Print "MilliSecs: " + (MilliSecs() - start)

        ' PRINT
        Print ""
        lispPrint(ret)
        Print ""
        gc()
    Until equ(ret, quit_val)
End Function

Function main()
'    nan_main()
'    lexer_main()
    parser_main()
End Function

main()