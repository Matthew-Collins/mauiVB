﻿using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.Maui.Controls.Internals;
using Microsoft.Maui.Graphics;

namespace Microsoft.Maui.Controls
{
	public partial class NavigationPage : Page, IPageContainer<Page>, IBarElement, IElementConfiguration<NavigationPage>
	{
		public static readonly BindableProperty BackButtonTitleProperty = BindableProperty.CreateAttached("BackButtonTitle", typeof(string), typeof(Page), null);

		public static readonly BindableProperty HasNavigationBarProperty =
			BindableProperty.CreateAttached("HasNavigationBar", typeof(bool), typeof(Page), true);

		public static readonly BindableProperty HasBackButtonProperty = BindableProperty.CreateAttached("HasBackButton", typeof(bool), typeof(NavigationPage), true);

		public static readonly BindableProperty BarBackgroundColorProperty = BarElement.BarBackgroundColorProperty;

		public static readonly BindableProperty BarBackgroundProperty = BarElement.BarBackgroundProperty;

		public static readonly BindableProperty BarTextColorProperty = BarElement.BarTextColorProperty;

		public static readonly BindableProperty TitleIconImageSourceProperty = BindableProperty.CreateAttached("TitleIconImageSource", typeof(ImageSource), typeof(NavigationPage), default(ImageSource));

		public static readonly BindableProperty IconColorProperty = BindableProperty.CreateAttached("IconColor", typeof(Color), typeof(NavigationPage), null);

		public static readonly BindableProperty TitleViewProperty = BindableProperty.CreateAttached("TitleView", typeof(View), typeof(NavigationPage), null, propertyChanging: TitleViewPropertyChanging);

		static readonly BindablePropertyKey CurrentPagePropertyKey = BindableProperty.CreateReadOnly("CurrentPage", typeof(Page), typeof(NavigationPage), null, propertyChanged: OnCurrentPageChanged);

		public static readonly BindableProperty CurrentPageProperty = CurrentPagePropertyKey.BindableProperty;

		static readonly BindablePropertyKey RootPagePropertyKey = BindableProperty.CreateReadOnly(nameof(RootPage), typeof(Page), typeof(NavigationPage), null);
		public static readonly BindableProperty RootPageProperty = RootPagePropertyKey.BindableProperty;

		INavigationPageController NavigationPageController => this;

		partial void Init();

		public NavigationPage() : this(
#if WINDOWS || ANDROID
			true
#else
			false
#endif
			)
		{
		}

		public NavigationPage(Page root) : this()
		{
			PushPage(root);
		}

		internal NavigationPage(bool setforMaui, Page root = null)
		{
			_platformConfigurationRegistry = new Lazy<PlatformConfigurationRegistry<NavigationPage>>(() => new PlatformConfigurationRegistry<NavigationPage>(this));

			if (setforMaui)
				Navigation = new MauiNavigationImpl(this);
			else
				Navigation = new NavigationImpl(this);

			Init();

			if (root != null)
				PushPage(root);
		}

		public Color BarBackgroundColor
		{
			get => (Color)GetValue(BarElement.BarBackgroundColorProperty);
			set => SetValue(BarElement.BarBackgroundColorProperty, value);
		}

		public Brush BarBackground
		{
			get => (Brush)GetValue(BarElement.BarBackgroundProperty);
			set => SetValue(BarElement.BarBackgroundProperty, value);
		}

		public Color BarTextColor
		{
			get => (Color)GetValue(BarElement.BarTextColorProperty);
			set => SetValue(BarElement.BarTextColorProperty, value);
		}

		internal Task CurrentNavigationTask { get; set; }

		[EditorBrowsable(EditorBrowsableState.Never)]
		public Page Peek(int depth)
		{
			if (depth < 0)
			{
				return null;
			}

			if (InternalChildren.Count <= depth)
			{
				return null;
			}

			return (Page)InternalChildren[InternalChildren.Count - depth - 1];
		}

		IEnumerable<Page> INavigationPageController.Pages => InternalChildren.Cast<Page>();

		int INavigationPageController.StackDepth
		{
			get { return InternalChildren.Count; }
		}

		public Page CurrentPage
		{
			get { return (Page)GetValue(CurrentPageProperty); }
			private set { SetValue(CurrentPagePropertyKey, value); }
		}

		public Page RootPage
		{
			get { return (Page)GetValue(RootPageProperty); }
			private set { SetValue(RootPagePropertyKey, value); }
		}

		static void TitleViewPropertyChanging(BindableObject bindable, object oldValue, object newValue)
		{
			if (oldValue == newValue)
				return;

			if (bindable is Page page)
			{
				page.SetTitleView((View)oldValue, (View)newValue);
			}
			else if (oldValue != null)
			{
				var oldElem = (View)oldValue;
				oldElem.Parent = null;
			}
		}

		public static string GetBackButtonTitle(BindableObject page)
		{
			return (string)page.GetValue(BackButtonTitleProperty);
		}

		public static bool GetHasBackButton(Page page)
		{
			if (page == null)
				throw new ArgumentNullException("page");
			return (bool)page.GetValue(HasBackButtonProperty);
		}

		public static bool GetHasNavigationBar(BindableObject page)
		{
			return (bool)page.GetValue(HasNavigationBarProperty);
		}

		public static ImageSource GetTitleIconImageSource(BindableObject bindable)
		{
			return (ImageSource)bindable.GetValue(TitleIconImageSourceProperty);
		}

		public static View GetTitleView(BindableObject bindable)
		{
			return (View)bindable.GetValue(TitleViewProperty);
		}

		public static Color GetIconColor(BindableObject bindable)
		{
			if (bindable == null)
			{
				return null;
			}

			return (Color)bindable.GetValue(IconColorProperty);
		}

		public Task<Page> PopAsync()
		{
			return PopAsync(true);
		}

		public async Task<Page> PopAsync(bool animated)
		{
			// If Navigation interactions are being handled by the MAUI APIs
			// this routes the call there instead of through old behavior
			if (Navigation is MauiNavigationImpl mvi && this is INavigationView)
			{
				return await mvi.PopAsync(animated);
			}

			var tcs = new TaskCompletionSource<bool>();
			try
			{
				if (CurrentNavigationTask != null && !CurrentNavigationTask.IsCompleted)
				{
					var oldTask = CurrentNavigationTask;
					CurrentNavigationTask = tcs.Task;
					await oldTask;
				}
				else
					CurrentNavigationTask = tcs.Task;

				var result = await (this as INavigationPageController).PopAsyncInner(animated, false);
				tcs.SetResult(true);
				return result;
			}
			catch (Exception e)
			{
				Log.Warning(nameof(NavigationPage), $"{e}");
				CurrentNavigationTask = null;
				tcs.SetCanceled();

				throw;
			}
		}

		public event EventHandler<NavigationEventArgs> Popped;

		public event EventHandler<NavigationEventArgs> PoppedToRoot;

		public Task PopToRootAsync()
		{
			return PopToRootAsync(true);
		}

		public async Task PopToRootAsync(bool animated)
		{
			// If Navigation interactions are being handled by the MAUI APIs
			// this routes the call there instead of through old behavior
			if (Navigation is MauiNavigationImpl mvi && this is INavigationView)
			{
				await mvi.PopToRootAsync(animated);
				return;
			}

			if (CurrentNavigationTask != null && !CurrentNavigationTask.IsCompleted)
			{
				var tcs = new TaskCompletionSource<bool>();
				Task oldTask = CurrentNavigationTask;
				CurrentNavigationTask = tcs.Task;
				await oldTask;

				await PopToRootAsyncInner(animated);
				tcs.SetResult(true);
				return;
			}

			Task result = PopToRootAsyncInner(animated);
			CurrentNavigationTask = result;
			await result;
		}

		public Task PushAsync(Page page)
		{
			return PushAsync(page, true);
		}

		public async Task PushAsync(Page page, bool animated)
		{	
			// If Navigation interactions are being handled by the MAUI APIs
			// this routes the call there instead of through old behavior
			if (Navigation is MauiNavigationImpl mvi && this is INavigationView)
			{
				if (InternalChildren.Contains(page))
					return;

				await mvi.PushAsync(page, animated);
				return;
			}

			if (CurrentNavigationTask != null && !CurrentNavigationTask.IsCompleted)
			{
				var tcs = new TaskCompletionSource<bool>();
				Task oldTask = CurrentNavigationTask;
				CurrentNavigationTask = tcs.Task;
				await oldTask;

				await PushAsyncInner(page, animated);
				tcs.SetResult(true);
				return;
			}

			CurrentNavigationTask = PushAsyncInner(page, animated);
			await CurrentNavigationTask;
		}

		public event EventHandler<NavigationEventArgs> Pushed;

		public static void SetBackButtonTitle(BindableObject page, string value)
		{
			page.SetValue(BackButtonTitleProperty, value);
		}

		public static void SetHasBackButton(Page page, bool value)
		{
			if (page == null)
				throw new ArgumentNullException("page");
			page.SetValue(HasBackButtonProperty, value);
		}

		public static void SetHasNavigationBar(BindableObject page, bool value)
		{
			page.SetValue(HasNavigationBarProperty, value);
		}

		public static void SetTitleIconImageSource(BindableObject bindable, ImageSource value)
		{
			bindable.SetValue(TitleIconImageSourceProperty, value);
		}

		public static void SetTitleView(BindableObject bindable, View value)
		{
			bindable.SetValue(TitleViewProperty, value);
		}

		public static void SetIconColor(BindableObject bindable, Color value)
		{
			bindable.SetValue(IconColorProperty, value);
		}

		protected override bool OnBackButtonPressed()
		{
			if (CurrentPage.SendBackButtonPressed())
				return true;

			if (NavigationPageController.StackDepth > 1)
			{
				SafePop();
				return true;
			}

			return base.OnBackButtonPressed();
		}

		internal void InitialNativeNavigationStackLoaded()
		{
			SendNavigated(null);
		}



		void SendNavigated(Page previousPage)
		{
			previousPage?.SendNavigatedFrom(new NavigatedFromEventArgs(CurrentPage));
			CurrentPage.SendNavigatedTo(new NavigatedToEventArgs(previousPage));
		}

		void SendNavigating(Page navigatingFrom = null)
		{
			(navigatingFrom ?? CurrentPage)?.SendNavigatingFrom(new NavigatingFromEventArgs());
		}


		void FireDisappearing(Page page)
		{
			if (HasAppeared)
				page?.SendDisappearing();
		}

		void FireAppearing(Page page)
		{
			if (HasAppeared)
				page?.SendAppearing();
		}


		void RemoveFromInnerChildren(Element page)
		{
			InternalChildren.Remove(page);
			page.Handler = null;
		}

		void SafePop()
		{
			PopAsync(true).ContinueWith(t =>
			{
				if (t.IsFaulted)
					throw t.Exception;
			});
		}

		readonly Lazy<PlatformConfigurationRegistry<NavigationPage>> _platformConfigurationRegistry;

		public new IPlatformElementConfiguration<T, NavigationPage> On<T>() where T : IConfigPlatform
		{
			return _platformConfigurationRegistry.Value.On<T>();
		}
	}
}
