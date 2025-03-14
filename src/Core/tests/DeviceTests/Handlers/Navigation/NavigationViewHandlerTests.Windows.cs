﻿using System;
using System.Threading.Tasks;
using Microsoft.Maui.DeviceTests.Stubs;
using Microsoft.Maui.Handlers;
using Microsoft.UI.Xaml;
using Microsoft.UI.Xaml.Controls;
using Xunit;

namespace Microsoft.Maui.DeviceTests
{
	public partial class NavigationViewHandlerTests
	{
		int GetNativeNavigationStackCount(NavigationViewHandler navigationViewHandler) =>
			navigationViewHandler.NativeView.BackStackDepth + 1;

		Task CreateNavigationViewHandlerAsync(INavigationView navigationView, Func<NavigationViewHandler, Task> action)
		{
			return InvokeOnMainThreadAsync(async () =>
			{
			FrameworkElement frameworkElement = null;
			var content = (Panel)DefaultWindow.Content;
			try
			{
				var mauiContext = new ContextStub(MauiContext.GetApplicationServices());
				var handler = CreateHandler(navigationView, mauiContext);
				frameworkElement = handler.NativeView;
				content.Children.Add(frameworkElement);
				if (navigationView is NavigationViewStub nvs && nvs.NavigationStack?.Count > 0)
				{
					navigationView.RequestNavigation(new NavigationRequest(nvs.NavigationStack, false));
					await nvs.OnNavigationFinished;
				}

				await action(handler);
			}
			finally
			{
				if (frameworkElement != null)
					content.Children.Remove(frameworkElement);
				}				
			});
		}
	}
}