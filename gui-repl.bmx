SuperStrict

Import MaxGui.Drivers

Type REPLWindow
    field padding:Int = 10
    field dbl_padding:Int  = padding * 2
    field btn_width:Int = 64
    field btn_height:Int = 24
    field window:TGadget   = CreateWindow("B-LISP REPL CLIENT", 0, 0, 800, 600)
    field scrollback_text:TGadget = CreateTextArea(padding, padding, window.width - dbl_padding, 240, window)
    field input_text:TGadget      = CreateTextArea(padding, GadgetY(scrollback_text) + GadgetHeight(scrollback_text), window.width - dbl_padding, 240, window)
    field send_btn:TGadget = CreateButton("Send", GadgetX(Input_text), GadgetY(Input_text) + GadgetHeight(Input_text) + padding, btn_width, btn_height, window)
    field quit_btn:TGadget  = CreateButton("Quit", GadgetX(send_btn) + GadgetWidth(send_btn) + padding, GadgetY(send_btn), btn_width, btn_height, window)

    Method prompt:String(prompt:String = "")
        AddTextAreaText(scrollback_text, prompt)
        While True
            WaitEvent() 
            
            Select EventID()
                Case EVENT_GADGETACTION
                    Select EventSource()
                        case send_btn
                            Return TextAreaText(Input_text)
                        case quit_btn
                            Return "(quit)"
                    End Select
            End Select
        Wend
    End Method

    Method addScrollbackText(text:String)
        AddTextAreaText(scrollback_text, text)
    End Method
    
End Type


