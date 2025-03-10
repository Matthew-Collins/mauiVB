#nullable enable
using Microsoft.UI.Xaml;
using Microsoft.UI.Xaml.Controls;
using Microsoft.UI.Xaml.Media;

namespace Microsoft.Maui.Handlers
{
	public partial class EditorHandler : ViewHandler<IEditor, MauiTextBox>
	{
		Brush? _placeholderDefaultBrush;
		Brush? _defaultPlaceholderColorFocusBrush;

		protected override MauiTextBox CreateNativeView() => new MauiTextBox
		{
			AcceptsReturn = true,
			TextWrapping = TextWrapping.Wrap,
			Style = Application.Current.Resources["MauiTextBoxStyle"] as Style,
			UpdateVerticalAlignmentOnLoad = false,
			VerticalContentAlignment = VerticalAlignment.Top
		};

		protected override void ConnectHandler(MauiTextBox nativeView)
		{
			nativeView.TextChanged += OnTextChanged;
			nativeView.LostFocus += OnLostFocus;
			SetupDefaults(nativeView);
		}

		protected override void DisconnectHandler(MauiTextBox nativeView)
		{
			nativeView.TextChanged -= OnTextChanged;
			nativeView.LostFocus -= OnLostFocus;
		}

		void SetupDefaults(MauiTextBox nativeView)
		{
			_placeholderDefaultBrush = nativeView.PlaceholderForeground;
			_defaultPlaceholderColorFocusBrush = nativeView.PlaceholderForegroundFocusBrush;
		}

		public static void MapText(EditorHandler handler, IEditor editor)
		{
			handler.NativeView?.UpdateText(editor);
		}

		public static void MapPlaceholder(EditorHandler handler, IEditor editor)
		{
			handler.NativeView?.UpdatePlaceholder(editor);
		}

		public static void MapPlaceholderColor(EditorHandler handler, IEditor editor)
		{
			handler.NativeView?.UpdatePlaceholderColor(editor, handler._placeholderDefaultBrush, handler._defaultPlaceholderColorFocusBrush);
		}

		public static void MapCharacterSpacing(EditorHandler handler, IEditor editor)
		{
			handler.NativeView?.UpdateCharacterSpacing(editor);
		}

		public static void MapMaxLength(EditorHandler handler, IEditor editor)
		{
			handler.NativeView?.UpdateMaxLength(editor);
		}

		public static void MapIsTextPredictionEnabled(EditorHandler handler, IEditor editor)
		{
			handler.NativeView?.UpdateIsTextPredictionEnabled(editor);
		}

		public static void MapFont(EditorHandler handler, IEditor editor)
		{
			var fontManager = handler.GetRequiredService<IFontManager>();

			handler.NativeView?.UpdateFont(editor, fontManager);
		}

		public static void MapIsReadOnly(EditorHandler handler, IEditor editor)
		{
			handler.NativeView?.UpdateIsReadOnly(editor);
		}

		public static void MapTextColor(EditorHandler handler, IEditor editor) =>
			handler.NativeView?.UpdateTextColor(editor);


		public static void MapHorizontalTextAlignment(EditorHandler handler, IEditor editor)
		{
			handler.NativeView?.UpdateHorizontalTextAlignment(editor);
		}

		public static void MapVerticalTextAlignment(EditorHandler handler, IEditor editor)
		{
			handler.NativeView?.UpdateVerticalTextAlignment(editor);
		}

		public static void MapKeyboard(EditorHandler handler, IEditor editor) 
		{
			handler.NativeView?.UpdateKeyboard(editor); 
		}

		void OnTextChanged(object sender, TextChangedEventArgs args)
		{
			VirtualView?.UpdateText(NativeView.Text);
		}

		void OnLostFocus(object? sender, RoutedEventArgs e)
		{
			VirtualView?.Completed();
		}
	}
}
