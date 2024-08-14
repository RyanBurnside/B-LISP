SuperStrict

Import "parse.bmx"
Import "GUI-REPL.bmx"

Function printBanner()
    Print ""
    Print ";     ######        #      ###  ######  ###### "
    Print ";    #     #       #       #  #        #     #"
    Print ";   ######  ##### #       #   ######  ###### "
    Print ";  #     #       #       #         # #      "
    Print "; ######        ###### ###  ######  #      "
    Print ""
    Print "; Ryan Burnside 2024 ver: Pre-Alpha"
    Print ""
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
    ' Local GUI:REPLWindow = New REPLWindow()
    printBanner()

    resetMachine()

    Repeat
        ' READ
        Local cellsUsed:ULong = N - sp
        Local cellsRemaining:ULong = sp - Ceil(hp / 8.0)
        Local statements:String = ""
        Local prompt:String = "B-LISP[" + cellsUsed + "/" + cellsRemaining + "]> "


        Repeat statements = Input(prompt) 'GUI.prompt(prompt)
        statements.Trim()
        Until statements <> ""

        Print "; Statements: " + statements
        Local lexer:Lexer = New Lexer(makeRegexTableBlisp(), statements)
        Local parser:Parser = New Parser(lexer)
        Local sexp:Double = parser.parse()
        prin "; Parsed Lisp Object: "
        lispPrint(sexp)
        Print ""

        ' EVAL
        ret = eval(sexp, env_val)

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
