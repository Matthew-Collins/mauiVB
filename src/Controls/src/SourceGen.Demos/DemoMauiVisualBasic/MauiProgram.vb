Imports Microsoft.Maui.Hosting
Imports Microsoft.Maui.Controls.Hosting

Namespace DemoMauiVisualBasic
    Public Module MauiProgram
        Public Function CreateMauiApp() As MauiApp
            Dim builder = MauiApp.CreateBuilder()
            builder.UseMauiApp(Of App)().ConfigureFonts(Sub(fonts) fonts.AddFont("OpenSans-Regular.ttf", "OpenSansRegular"))
            Return builder.Build()
        End Function
    End Module
End Namespace
