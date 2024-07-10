SuperStrict

Import brl.retro
Import Text.RegEx
Import Text.format

Const ATOM_TAG:ULong = $7ff8  'atom
Const PRIM_TAG:ULong = $7ff9  'primitive
Const CONS_TAG:ULong = $7ffa  'cons cell
Const CLOS_TAG:ULong = $7ffb  'closure
Const NIL_TAG:ULong  = $7ffc  'duh

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

Type Token
    Field typ:TokenType
    Field value:String
	' add line and column errors later (if one gives a shit)
    Global formatter:TFormatter = TFormatter.Create("<Token %s(%d), '%s' >")
    
    Method New(typ:TokenType, value:String)
        Self.typ = typ
        Self.value = value
    End Method
    
    Method Print()
        formatter.Clear()
        formatter.Arg(typ.ToString()).Arg(Int(typ)).Arg(value)
        Print formatter.format()
    End Method
End Type

' This is the regex table (to be transformed) for B-LISP
Function makeRegexTableBlisp:String[][] ()
    ' Regex groups, order by most specific to least
    Return [["SYMBOL",    "[A-Za-z!@#$%^&*-+\\<>]+"],
            ["NUMBER",    "\d+(\.\d*)?"],
            ["BACKQUOTE", "`"],
            ["SPLICE",    ",@"],
            ["COMMA",     ","],
            ["QUOTE",     "'"],
            ["LPAREN",    "\("],
            ["RPAREN",    "\)"],
            ["LDOT",       "\."],
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

Function main()
	Print "In main ..."
	Local statements:String = """
		(* (+ FOO *BAR* +BAZ+) |foobar| BAZ 42 43.5)
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

main()