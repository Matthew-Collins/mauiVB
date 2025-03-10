using System;
using System.Threading.Tasks;
using Microsoft.Maui.Dispatching;
using Microsoft.Maui.TestUtils.DeviceTests.Runners;

namespace Microsoft.Maui.DeviceTests
{
	public partial class TestBase
	{
		readonly Random rnd = new Random();

		protected Task<T> InvokeOnMainThreadAsync<T>(Func<T> func) =>
			TestDispatcher.Current.InvokeOnMainThreadAsync(func);

		protected Task InvokeOnMainThreadAsync(Action action) =>
			TestDispatcher.Current.InvokeOnMainThreadAsync(action);

		protected Task InvokeOnMainThreadAsync(Func<Task> action) =>
			TestDispatcher.Current.InvokeOnMainThreadAsync(action);

		protected Task<T> InvokeOnMainThreadAsync<T>(Func<Task<T>> func) =>
			TestDispatcher.Current.InvokeOnMainThreadAsync(func);

		protected async Task<bool> Wait(Func<bool> exitCondition, int timeout = 1000)
		{
			while ((timeout -= 100) > 0)
			{
				if (!exitCondition.Invoke())
					await Task.Delay(rnd.Next(100, 200));
				else
					break;
			}

			return exitCondition.Invoke();
		}
	}
}
