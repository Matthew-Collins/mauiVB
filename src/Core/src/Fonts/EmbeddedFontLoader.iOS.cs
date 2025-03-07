#nullable enable
using System;
using CoreGraphics;
using CoreText;
using Foundation;
using Microsoft.Extensions.Logging;
using UIKit;

namespace Microsoft.Maui
{
	public partial class EmbeddedFontLoader
	{
		public string? LoadFont(EmbeddedFont font)
		{
			try
			{
				if (font.ResourceStream == null)
					throw new InvalidOperationException("ResourceStream was null.");

				var data = NSData.FromStream(font.ResourceStream);
				if (data == null)
					throw new InvalidOperationException("Unable to load font stream data.");
				var provider = new CGDataProvider(data);
				var cgFont = CGFont.CreateFromProvider(provider);
				if (cgFont == null)
					throw new InvalidOperationException("Unable to load font from the stream.");

				var name = cgFont.PostScriptName;

				if (CTFontManager.RegisterGraphicsFont(cgFont, out var error))
					return name;

				var uiFont = UIFont.FromName(name, 10);
				if (uiFont != null)
					return name;

				throw new NSErrorException(error);
			}
			catch (Exception ex)
			{
				_logger?.LogWarning(ex, "Unable register font {Font} with the system.", font.FontName);
			}

			return null;
		}
	}
}