// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System;
using System.Collections.Specialized;
using System.ComponentModel;
using System.IO;
using System.Linq;
using System.Windows.Forms;
using Microsoft.AspNetCore.Components.WebView.WebView2;
using Microsoft.Extensions.FileProviders;
using WebView2Control = Microsoft.Web.WebView2.WinForms.WebView2;

namespace Microsoft.AspNetCore.Components.WebView.WindowsForms
{
	/// <summary>
	/// A Windows Forms control for hosting Blazor web components locally in Windows desktop applications.
	/// </summary>
	public class BlazorWebView : ContainerControl
	{
		private readonly WebView2Control _webview;
		private WebView2WebViewManager _webviewManager;
		private string _hostPage;
		private IServiceProvider _services;

		/// <summary>
		/// Creates a new instance of <see cref="BlazorWebView"/>.
		/// </summary>
		public BlazorWebView()
		{
			ComponentsDispatcher = new WindowsFormsDispatcher(this);

			RootComponents.CollectionChanged += HandleRootComponentsCollectionChanged;

			_webview = new WebView2Control()
			{
				Dock = DockStyle.Fill,
			};
			((BlazorWebViewControlCollection)Controls).AddInternal(_webview);
		}

		/// <summary>
		/// Returns the inner <see cref="WebView2Control"/> used by this control.
		/// </summary>
		/// <remarks>
		/// Directly using some functionality of the inner web view can cause unexpected results because its behavior
		/// is controlled by the <see cref="BlazorWebView"/> that is hosting it.
		/// </remarks>
		[Browsable(false)]
		[DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)]
		public WebView2Control WebView => _webview;

		/// <summary>
		/// Returns the current <see cref="WebView2WebViewManager"/> used by this control. This property is <c>null</c>
		/// until after the XYZ event is raised.
		/// </summary>
		public WebView2WebViewManager WebViewManager => _webviewManager;

		private WindowsFormsDispatcher ComponentsDispatcher { get; }

		/// <inheritdoc />
		protected override void OnCreateControl()
		{
			base.OnCreateControl();

			StartWebViewCoreIfPossible();
		}

		/// <summary>
		/// Path to the host page within the application's static files. For example, <code>wwwroot\index.html</code>.
		/// This property must be set to a valid value for the Blazor components to start.
		/// </summary>
		[Category("Behavior")]
		[Description(@"Path to the host page within the application's static files. Example: wwwroot\index.html.")]
		public string HostPage
		{
			get => _hostPage;
			set
			{
				_hostPage = value;
				OnHostPagePropertyChanged();
			}
		}

		/// <summary>
		/// Occurs when the <see cref="WebView2WebViewManager"/> is created.
		/// </summary>
		public event EventHandler<WebViewManagerCreatedEventArgs> WebViewManagerCreated;

		protected virtual void OnWebViewManagerCreated(WebViewManagerCreatedEventArgs webViewManagerCreatedEventArgs)
		{
			WebViewManagerCreated?.Invoke(this, webViewManagerCreatedEventArgs);
		}

		// Learn more about these methods here: https://docs.microsoft.com/en-us/dotnet/desktop/winforms/controls/defining-default-values-with-the-shouldserialize-and-reset-methods?view=netframeworkdesktop-4.8
		private void ResetHostPage() => HostPage = null;
		private bool ShouldSerializeHostPage() => !string.IsNullOrEmpty(HostPage);

		/// <summary>
		/// A collection of <see cref="RootComponent"/> instances that specify the Blazor <see cref="IComponent"/> types
		/// to be used directly in the specified <see cref="HostPage"/>.
		/// </summary>
		[Browsable(false)]
		[DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)]
		public RootComponentsCollection RootComponents { get; } = new();

		/// <summary>
		/// Gets or sets an <see cref="IServiceProvider"/> containing services to be used by this control and also by application code.
		/// This property must be set to a valid value for the Blazor components to start.
		/// </summary>
		[Browsable(false)]
		[DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)]
		public IServiceProvider Services
		{
			get => _services;
			set
			{
				_services = value;
				OnServicesPropertyChanged();
			}
		}

		private void OnHostPagePropertyChanged() => StartWebViewCoreIfPossible();

		private void OnServicesPropertyChanged() => StartWebViewCoreIfPossible();

		private bool RequiredStartupPropertiesSet =>
			Created &&
			_webview != null &&
			HostPage != null &&
			Services != null;

		private void StartWebViewCoreIfPossible()
		{
			// We never start the Blazor code in design time because it doesn't make sense to run
			// a Blazor component in the designer.
			if (IsAncestorSiteInDesignMode)
			{
				return;
			}

			// If we don't have all the required properties, or if there's already a WebViewManager, do nothing
			if (!RequiredStartupPropertiesSet || _webviewManager != null)
			{
				return;
			}

			// We assume the host page is always in the root of the content directory, because it's
			// unclear there's any other use case. We can add more options later if so.
			var contentRootDir = Path.GetDirectoryName(Path.GetFullPath(HostPage));
			var hostPageRelativePath = Path.GetRelativePath(contentRootDir, HostPage);

			var customFileProvider = CreateFileProvider(contentRootDir);
			var assetFileProvider = new PhysicalFileProvider(contentRootDir);
			IFileProvider fileProvider = customFileProvider == null
				? assetFileProvider
				: new CompositeFileProvider(customFileProvider, assetFileProvider);

			_webviewManager = new WebView2WebViewManager(new WindowsFormsWebView2Wrapper(_webview), Services, ComponentsDispatcher, fileProvider, RootComponents.JSComponents, hostPageRelativePath);

			foreach (var rootComponent in RootComponents)
			{
				// Since the page isn't loaded yet, this will always complete synchronously
				_ = rootComponent.AddToWebViewManagerAsync(_webviewManager);
			}
			_webviewManager.Navigate("/");
		}

		private void HandleRootComponentsCollectionChanged(object sender, NotifyCollectionChangedEventArgs eventArgs)
		{
			// If we haven't initialized yet, this is a no-op
			if (_webviewManager != null)
			{
				// Dispatch because this is going to be async, and we want to catch any errors
				_ = ComponentsDispatcher.InvokeAsync(async () =>
				{
					var newItems = eventArgs.NewItems.Cast<RootComponent>();
					var oldItems = eventArgs.OldItems.Cast<RootComponent>();

					foreach (var item in newItems.Except(oldItems))
					{
						await item.AddToWebViewManagerAsync(_webviewManager);
					}

					foreach (var item in oldItems.Except(newItems))
					{
						await item.RemoveFromWebViewManagerAsync(_webviewManager);
					}
				});
			}
		}

		/// <summary>
		/// Creates a file provider for static assets used in the <see cref="BlazorWebView"/>. Override
		/// this method to return a custom <see cref="IFileProvider"/> to serve assets such as <c>wwwroot/index.html</c>.
		/// </summary>
		/// <param name="contentRootDir">The base directory to use for all requested assets, such as <c>wwwroot</c>.</param>
		/// <returns>Returns a <see cref="IFileProvider"/> for static assets, or <c>null</c> if there is no custom provider.</returns>
		public virtual IFileProvider CreateFileProvider(string contentRootDir)
		{
			return null;
		}

		/// <inheritdoc />
		protected override void Dispose(bool disposing)
		{
			if (disposing)
			{
				// Dispose this component's contents and block on completion so that user-written disposal logic and
				// Blazor disposal logic will complete first. Then call base.Dispose(), which will dispose the WebView2
				// control. This order is critical because once the WebView2 is disposed it will prevent and Blazor
				// code from working because it requires the WebView to exist.
				_webviewManager?
					.DisposeAsync()
					.AsTask()
					.GetAwaiter()
					.GetResult();
			}
			base.Dispose(disposing);
		}

		/// <inheritdoc />
		protected override ControlCollection CreateControlsInstance()
		{
			return new BlazorWebViewControlCollection(this);
		}

		/// <summary>
		/// Custom control collection that ensures that only the owning <see cref="BlazorWebView"/> can add
		/// controls to it.
		/// </summary>
		private sealed class BlazorWebViewControlCollection : ControlCollection
		{
			public BlazorWebViewControlCollection(BlazorWebView owner) : base(owner)
			{
			}

			/// <summary>
			/// This is the only API we use; everything else is blocked.
			/// </summary>
			/// <param name="value"></param>
			internal void AddInternal(Control value) => base.Add(value);

			// Everything below is overridden to protect the control collection as read-only.
			public override bool IsReadOnly => true;

			public override void Add(Control value) => throw new NotSupportedException();
			public override void Clear() => throw new NotSupportedException();
			public override void Remove(Control value) => throw new NotSupportedException();
			public override void SetChildIndex(Control child, int newIndex) => throw new NotSupportedException();
		}
	}
}
