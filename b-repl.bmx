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
    ' Local GUI:REPLWindow = New REPLWindow()
    printBanner()

    resetMachine()

    Repeat
        ' READ
        Local cellsUsed:ULong = N - sp
        Local cellsRemaining:ULong = sp - Ceil(hp / 8.0)
        Local statements:String = ""
        Local prompt:String = "B-LISP[" + cellsUsed + "/" + cellsRemaining + "]> "

        Repeat statements = Input(prompt) ' GUI.prompt(prompt)
            statements.Trim()
        Until statements <> ""

        Print "; Statements: " + statements
        Local lexer:Lexer = New Lexer(makeRegexTableBlisp(), statements)
        Local parser:Parser = New Parser(lexer)
        Local sexp:Double = parser.parse()
        prin "; Parsed Lisp Object: "
        lispPrint(sexp)
        Print ""

        prin "; Parsed Lisp Object in API Form: "
        prin "; "
        apiPrint(sexp)
        Print ""

        ' EVAL
        ret = eval(sexp, env_val)

        ' PRINT
        Print "; This is Lisp Print"
        Print ""
        lispPrint(ret)
        Print "~n"

        Print "; This is Api Print"
        Print ""
        prin "; "
        apiPrint(ret)
        Print "~n"

        gc()
    Until equ(ret, quit_val)
End Function

Function main()
    parser_main()
End Function

main()
