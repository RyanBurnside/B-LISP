need To run toSuperStrict
Import brl.map

' two simple sample functions

Function foo:String(name:String)
    Return "Function foo called with value: " + name
End Function

Function bar:String(name:String)
    Return "Function bar called with value: " + name
End Function

' Test a hash of functions
Type fnWrapper
	Field fn:String(s:String)
	Method New (fn:String(s:String))
		Self.fn = fn
	End Method
End Type

Local map:TStringMap = New TStringMap
map["foo"] = New fnWrapper(foo)
map["bar"] = New fnWrapper(bar)

Print fnWrapper(map["foo"]).fn("HELLO FROM MAP")
Print fnWrapper(map["bar"]).fn("HELLO FROM MAP")

' test traversing an array of anon fns
Local v:String(s:String)[] = [foo, foo, foo]

For Local i:String(s:String) = EachIn v
	Print i("RYAN")
Next