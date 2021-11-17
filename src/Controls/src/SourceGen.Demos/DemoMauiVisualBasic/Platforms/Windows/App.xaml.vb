Imports Microsoft.Maui
Imports Microsoft.Maui.Hosting
Imports Microsoft.UI.Xaml
Imports Windows.ApplicationModel

' To learn more about WinUI, the WinUI project structure,
' and more about our project templates, see: http://aka.ms/winui-project-info.

Namespace DemoMauiVisualBasic.WinUI
    ''' <summary>
    ''' Provides application-specific behavior to supplement the default Application class.
    ''' </summary>
    Public Partial Class App
        Inherits MauiWinUIApplication
        ''' <summary>
        ''' Initializes the singleton application object.  This is the first line of authored code
        ''' executed, and as such is the logical equivalent of main() or WinMain().
        ''' </summary>
        Public Sub New()
            Me.InitializeComponent()
        End Sub

        Protected Overrides Function CreateMauiApp() As MauiApp
            Return MauiProgram.CreateMauiApp()
        End Function

        Protected Overrides Sub OnLaunched(ByVal args As LaunchActivatedEventArgs)
            MyBase.OnLaunched(args)
            Microsoft.Maui.Essentials.Platform.OnLaunched(args)
        End Sub
    End Class
End Namespace
