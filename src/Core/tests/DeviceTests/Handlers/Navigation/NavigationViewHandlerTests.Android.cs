﻿using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using Android.OS;
using Android.Views;
using AndroidX.AppCompat.App;
using AndroidX.AppCompat.Widget;
using AndroidX.Fragment.App;
using Microsoft.Maui.DeviceTests.Stubs;
using Microsoft.Maui.Handlers;
using Xunit;
using ATextAlignment = Android.Views.TextAlignment;

namespace Microsoft.Maui.DeviceTests
{
	public partial class NavigationViewHandlerTests
	{
		int GetNativeNavigationStackCount(NavigationViewHandler navigationViewHandler) =>
			navigationViewHandler.StackNavigationManager.NavHost.NavController.BackStack.Size() - 1;

		Task CreateNavigationViewHandlerAsync(INavigationView navigationView, Func<NavigationViewHandler, Task> action)
		{
			return InvokeOnMainThreadAsync(async () =>
			{
				ViewGroup rootView = (DefaultContext as AppCompatActivity).Window.DecorView as ViewGroup;
				var linearLayoutCompat = new LinearLayoutCompat(DefaultContext);

				try
				{
					var mauiContext = new ContextStub(MauiContext.GetApplicationServices());
					var viewFragment = new NavViewFragment(mauiContext);


					var fragmentManager = (DefaultContext as AppCompatActivity).GetFragmentManager();
					linearLayoutCompat.Id = View.GenerateViewId();

					fragmentManager
						.BeginTransaction()
						.Add(linearLayoutCompat.Id, viewFragment)
						.Commit();

					rootView.AddView(linearLayoutCompat);
					await viewFragment.FinishedLoading;
					var handler = CreateHandler(navigationView, viewFragment.ScopedMauiContext);

					if (navigationView is NavigationViewStub nvs && nvs.NavigationStack?.Count > 0)
					{
						navigationView.RequestNavigation(new NavigationRequest(nvs.NavigationStack, false));
						await nvs.OnNavigationFinished;
					}

					await action(handler);
				}
				finally
				{
					rootView.RemoveView(linearLayoutCompat);
				}
			});
		}



		class NavViewFragment : Fragment
		{
			TaskCompletionSource<bool> _taskCompletionSource = new TaskCompletionSource<bool>();
			readonly IMauiContext _mauiContext;
			public IMauiContext ScopedMauiContext { get; set; }

			public Task FinishedLoading => _taskCompletionSource.Task;
			public NavViewFragment(IMauiContext mauiContext)
			{
				_mauiContext = mauiContext;
			}

			public override View OnCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState)
			{
				ScopedMauiContext = _mauiContext.MakeScoped(layoutInflater: inflater, fragmentManager: ChildFragmentManager);
				return ScopedMauiContext.GetNavigationRootManager().RootView;
			}

			public override void OnResume()
			{
				base.OnResume();
				_taskCompletionSource.SetResult(true);
			}
		}
	}
}