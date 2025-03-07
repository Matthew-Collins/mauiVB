using System.ComponentModel;
using Microsoft.Maui.Graphics;
using Microsoft.UI.Xaml;
using Microsoft.UI.Xaml.Automation.Peers;
using Microsoft.UI.Xaml.Controls;
using Microsoft.Maui.Controls.Platform;
using WBorder = Microsoft.UI.Xaml.Controls.Border;

namespace Microsoft.Maui.Controls.Compatibility.Platform.UWP
{
	public class FrameRenderer : ViewRenderer<Frame, WBorder>
	{
		public FrameRenderer()
		{
			AutoPackage = false;
		}

		protected override AutomationPeer OnCreateAutomationPeer()
		{
			// We need an automation peer so we can interact with this in automated tests
			if (Control == null)
			{
				return new FrameworkElementAutomationPeer(this);
			}

			return new FrameworkElementAutomationPeer(Control);
		}

		protected override void OnElementChanged(ElementChangedEventArgs<Frame> e)
		{
			base.OnElementChanged(e);

			if (e.NewElement != null)
			{
				if (Control == null)
					SetNativeControl(new WBorder());

				PackChild();
				UpdateBorder();
				UpdateCornerRadius();
			}
		}

		protected override void OnElementPropertyChanged(object sender, PropertyChangedEventArgs e)
		{
			base.OnElementPropertyChanged(sender, e);

			if (e.PropertyName == "Content")
			{
				PackChild();
			}
			else if (e.PropertyName == Frame.BorderColorProperty.PropertyName || e.PropertyName == Frame.HasShadowProperty.PropertyName)
			{
				UpdateBorder();
			}
			else if (e.PropertyName == Frame.CornerRadiusProperty.PropertyName)
			{
				UpdateCornerRadius();
			}
		}

		protected override void UpdateBackgroundColor()
		{
			// Background color change must be handled separately
			// because the background would protrude through the border if the corners are rounded
			// as the background would be applied to the renderer's FrameworkElement
			Color backgroundColor = Element.BackgroundColor;

			if (Control != null)
			{
				Control.Background = backgroundColor.IsDefault() ?
					new Microsoft.UI.Xaml.Media.SolidColorBrush((global::Windows.UI.Color)Resources["SystemAltHighColor"]) : Maui.ColorExtensions.ToNative(backgroundColor);
			}
		}

		protected override void UpdateBackground()
		{
			Color backgroundColor = Element.BackgroundColor;
			Brush background = Element.Background;

			if (Control != null)
			{
				if (Brush.IsNullOrEmpty(background))
					Control.Background = backgroundColor.IsDefault() ?
						new Microsoft.UI.Xaml.Media.SolidColorBrush((global::Windows.UI.Color)Resources["SystemAltHighColor"]) : Maui.ColorExtensions.ToNative(backgroundColor);
				else
					Control.Background = background.ToBrush();
			}
		}

		void PackChild()
		{
			if (Element.Content == null)
				return;

			IVisualElementRenderer renderer = Element.Content.GetOrCreateRenderer();
			Control.Child = renderer.ContainerElement;
		}

		void UpdateBorder()
		{
			if (Element.BorderColor.IsNotDefault())
			{
				Control.BorderBrush = Maui.ColorExtensions.ToNative(Element.BorderColor);
				Control.BorderThickness = WinUIHelpers.CreateThickness(1);
			}
			else
			{
				Control.BorderBrush = Maui.ColorExtensions.ToNative(new Color(0, 0, 0, 0));
			}
		}

		void UpdateCornerRadius()
		{
			float cornerRadius = Element.CornerRadius;

			if (cornerRadius == -1f)
				cornerRadius = 5f; // default corner radius

			Control.CornerRadius = WinUIHelpers.CreateCornerRadius(cornerRadius);
		}
	}
}