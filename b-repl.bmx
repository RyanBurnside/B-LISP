SuperStrict

Import "parse.bmx"
' Import "gui-repl.bmx" ' there seems To be a library missing in Release mode.

Function printBanner()
    Print ""
    Print ";     ######        #      ###  ######  ###### "
    Print ";    #     #       #       #  #        #     #"
    Print ";   ######  ##### #       #   ######  ###### "
    Print ";  #     #       #       #         # #      "
    Print "; ######        ###### ###  ######  #      "
    Print ""
    Print "; Ryan Burnside 1976 ver: Pre-Alpha"
    Print "; Enter (quit) to leave the REPL.~n"
End Function

Function parser_main()
    Local ret:Double
    Local b:Blisp = New Blisp() 'make an instance with default prim table
    ' Local GUI:REPLWindow = New REPLWindow()
    printBanner()

    b.resetMachine()

    Repeat
        ' READ
        Local cellsUsed:ULong = b.N - b.sp
        Local cellsRemaining:ULong = b.sp - Ceil(b.hp / 8.0)
        Local statements:String = ""
        Local prompt:String = "B-LISP[" + cellsUsed + "/" + cellsRemaining + "]> "

        Repeat statements = Input(prompt) ' GUI.prompt(prompt)
            statements.Trim()
        Until statements <> ""

        Print "; Statements: " + statements
        Local lexer:Lexer = New Lexer(makeRegexTableBlisp(), statements)
        Local parser:Parser = New Parser(b, lexer)
        Local sexp:Double = parser.parse()
        b.prin "; Parsed Lisp Object: "
        b.lispPrint(sexp)
        Print ""

        b.prin "; Parsed Lisp Object in API Form: "
        b.prin "; "
        b.apiPrint(sexp)
        Print ""

        ' EVAL
        ret = b.eval(sexp, b.env_val)

        ' PRINT
        Print "; This is Lisp Print"
        Print ""
        b.lispPrint(ret)
        Print "~n"

        Print "; This is Api Print"
        Print ""
        b.prin "; "
        b.apiPrint(ret)
        Print "~n"

        b.gc()
    Until b.equ(ret, b.quit_val)
End Function

Function main()
    parser_main()
End Function

main()
