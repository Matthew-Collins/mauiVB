﻿Imports UIKit

Namespace DemoMauiVisualBasic
    Public Class Program
        ' This is the main entry point of the application.
        Private Shared Sub Main(ByVal args As String())
            ' if you want to use a different Application Delegate class from "AppDelegate"
            ' you can specify it here.
            UIApplication.Main(args, Nothing, GetType(AppDelegate))
        End Sub
    End Class
End Namespace
