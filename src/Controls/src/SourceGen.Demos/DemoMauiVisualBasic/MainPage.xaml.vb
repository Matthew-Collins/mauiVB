Imports System
Imports Microsoft.Maui.Controls
Imports Microsoft.Maui.Essentials

Namespace DemoMauiVisualBasic
    Public Partial Class MainPage
        Inherits ContentPage

        Private count As Integer = 0

        Public Sub New()
            Me.InitializeComponent()
        End Sub

        Private Sub OnCounterClicked(ByVal sender As Object, ByVal e As EventArgs)
            count += 1
            Me.CounterLabel.Text = $"Current count: {count}"
            SemanticScreenReader.Announce(Me.CounterLabel.Text)
        End Sub
    End Class
End Namespace
