SuperStrict

Import brl.retro
Import Text.RegEx
Import Text.format

Include "NaN.bmx"

Type Token
    Field typ:String
    Field value:String
    Field line:Int
	Field column:Int
	Global formatter:TFormatter = TFormatter.Create("<Token typ:%s, value:%s, line:%d, column:%d>")
	
	Method New(typ:String, value:String, line:Int, column:Int)
		Self.typ = typ
		Self.value = value
		Self.line = line
		Self.column = column
	End Method
	
	Method Print()
		formatter.Clear()
		formatter.Arg(typ).Arg(value).Arg(line).Arg(column)
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
		    ["DOT",       "\."],
            ["SKIP",      "[ \t]"]]
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

Function Tokenize(regexTable:String[][], s:String)
	' Regex documentation https://www.blitzmax.org/docs/en/api/text/text.regex/
	
	' Regex Class
	Local getToken:TRegEx = RegexTableToRegex(regexTable)
	
	' Regex match class
	Local matcher:TRegExMatch = getToken.Find(s)
	
	' Tokenization details
	Local line:Int = 1
	Local pos:Int = 0
	Local lineStart:Int = 0
	
	' Collects tokens from list
	Local tokenList:TList = New TList
	
	While matcher
        For Local n:String[] = EachIn regexTable
			Local captureName:String = n[0]
			Local matched:String = matcher.SubExpByName(captureName)
			If matched <> "" And captureName <> "SKIP"
				tokenList.AddLast(New Token(captureName, matched, -1, -1))
			    Exit ' early break, 1 match per regex (most specific)
			End If 
        Next
        matcher = getToken.Find()
    Wend

	For Local t:Token = EachIn(tokenList)
		t.Print()
	Next
End Function

Local statements:String = """
    (* (+ FOO *BAR* +BAZ+) BAZ 42 43.5)
    `(,SYM ,@(1 2 3 4))
     '(3 . 4)
"""

Tokenize(makeRegexTableBlisp(), statements)

Local LISPOBJECTS:Double[8192]
Print SizeOf(LISPOBJECTS)