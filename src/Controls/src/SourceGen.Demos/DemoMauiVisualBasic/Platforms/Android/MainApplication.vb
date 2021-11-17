Imports System
Imports Android.App
Imports Android.Runtime
Imports Microsoft.Maui
Imports Microsoft.Maui.Hosting

Namespace DemoMauiVisualBasic
    <Application>
    Public Class MainApplication
        Inherits MauiApplication

        Public Sub New(ByVal handle As IntPtr, ByVal ownership As JniHandleOwnership)
            MyBase.New(handle, ownership)
        End Sub

        Protected Overrides Function CreateMauiApp() As MauiApp
            Return MauiProgram.CreateMauiApp()
        End Function
    End Class
End Namespace
