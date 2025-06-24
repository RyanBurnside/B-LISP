SuperStrict

Import brl.retro
Import text.format
Import Text.RegEx
Import "b-lisp.bmx"

' this needs To be taken out later as Parser.bmx

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
    LAZYCLOSE, ' ] closes all trailing parens ending Exp TODO implement [
    TEXT_EOF
End Enum


Function stringToTokenType:TokenType(str:String)
    Select str
    Case "ATOM"      Return TokenType.SYMBOL
    Case "SYMBOL"    Return TokenType.SYMBOL
    Case "NUMBER"    Return TokenType.NUMBER
    Case "BACKQUOTE" Return TokenType.BACKQUOTE
    Case "SPLICE"    Return TokenType.SPLICE
    Case "COMMA"     Return TokenType.COMMA
    Case "QUOTE"     Return TokenType.QUOTE
    Case "LPAREN"    Return TokenType.LPAREN
    Case "RPAREN"    Return TokenType.RPAREN
    Case "LDOT"      Return TokenType.LDOT
    Case "SKIP"      Return TokenType.SKIP
    Case "LAZYCLOSE" Return TokenType.LAZYCLOSE
    Case "ERROR"     Return TokenType.ERROR
    Default          Return TokenType.ERROR
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
            ["LAZYCLOSE", "\]"],
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
    Field nestDepth:Double = 0
    Field b:Blisp
    
    Method New(b:Blisp, lexer:Lexer)
        Self.lexer = lexer
        Self.b = b
        currTok = lexer.NextToken()
    End Method

    Method match:Byte(t:TokenType)
        Return t = currTok.typ
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
            nestDepth :+ 1
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
        Return b.num(Double(numTok.value))
    End Method

    Method parseSymbol:Double()
        Local symTok:Token = accept()
        Return b.atom(symTok.value)
     End Method

    Method parseList:Double()
        Local expr:Double

        If match(TokenType.LAZYCLOSE)
            nestDepth :- 1
            If nestDepth = 0 Then accept()
            Return b.nil_val
        End If

        If match(TokenType.RPAREN)
            accept() ' Tosses right paren
            Return b.nil_val
        End If

        If match(TokenType.LDOT)
            accept() ' Tosses the LDOT
            expr:Double = parse()
            If match(TokenType.RPAREN) Then accept Else error
            Return expr
        End If

        expr:Double = parse()
        Return b.cons(expr, parseList())
    End Method

    Method parseQuote:Double()
        Return b.cons(b.atom("quote"), b.cons(parse(), b.nil_val))
    End Method
End Type
