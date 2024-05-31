SuperStrict

Import brl.map
Import Text.RegEx
Import Text.format

' NaN boxing
' |s b b b b | b b b b | b b b b | b b b b | ... | b b b b |
'    <- expt ------------------>   <- fraction ---------->
'                                    <tag>   < also avail>

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

Function Tokenize:Token[](s:String)
	' Regex documentation https://www.blitzmax.org/docs/en/api/text/text.regex/

    ' Accessory fn to transform the tokenSpecification arrays into a final regex
	Function JoinTokenSpecs:String(ID:String, Regex:String)
		Global regPair:TFormatter = TFormatter.Create("(?P<%s>%s)")
		regPair.clear()
		regPair.Arg(ID).Arg(Regex)
		Return regPair.format()
	End Function
	
	' Regex groups, order by most specific to least
	Local tokenSpecification:String[][] = .. 'TODO make these second strings /truly/ regex strings
	    [["SYMBOL",    "[A-Za-z!@#$%^&*-+\\<>]+"],
	     ["NUMBER",    "\d+(\.\d*)?"],
		 ["BACKQUOTE", "`"],
		 ["SPLICE",    ",@"],
		 ["COMMA",     ","],
		 ["QUOTE",     "'"],
         ["LPAREN",    "\("],
         ["RPAREN",    "\)"],       
		 ["DOT",       "\."],
         ["SKIP",      "[ \t]"]]

	Local temp:String[tokenSpecification.Length]
	Local counter:Int = 0
	
	For Local i:String[] = EachIn tokenSpecification
		temp[counter] = JoinTokenSpecs(i[0], i[1])
		counter = counter + 1
	Next
	
	Local list:TList = New TList
	' Multi-Part joined Regex String for token
	Local tokRegex:String = "|".join(temp)
	
	' Regex Class
	Local getToken:TRegEx = TRegEx.Create(tokRegex)
	
	' Regex match class
	Local match:TRegExMatch = getToken.Find(s)
	
	' Tokenization details
	Local line:Int = 1
	Local pos:Int = 0
	Local lineStart:Int = 0
	
	' Collects tokens from list
	Local tokenList:TList = New TList
	
	While match
        For Local n:String = EachIn ["SYMBOL", "NUMBER", "BACKQUOTE", "SPLICE", "COMMA", "QUOTE", "LPAREN", "RPAREN", "DOT", "SKIP"]
			Local m:String = match.SubExpByName(n)
			If m <> "" And n <> "SKIP"
				tokenList.AddLast(New Token(n, m, -1, -1))
			    Exit ' early break, 1 match per regex
			End If 
        Next
        match = getToken.Find()
    Wend

	For Local t:Token = EachIn(tokenList)
		t.Print()
	Next
End Function

Local statements:String = """
    (* (+ FOO *BAR* +BAZ+ ) BAZ 42 43.5)
    `(,SYM ,@(1 2 3 4))
     '(3 . 4)
    
"""

Tokenize(statements)




	
	
	
