﻿using NUnit.Framework;

namespace Microsoft.Maui.Controls.Core.UnitTests.Layouts
{
	public class ElementTests : BaseTestFixture 
	{
		class CustomLayout : StackLayout 
		{
			public int AddedCount { get; set; }
			public int RemovedCount { get; set; }

			protected override void OnChildAdded(Element child)
			{
				base.OnChildAdded(child);
				AddedCount += 1;
			}

			protected override void OnChildRemoved(Element child, int oldLogicalIndex)
			{
				base.OnChildRemoved(child, oldLogicalIndex);
				RemovedCount += 1;
			}
		}

		[Test]
		public void OnChildAddedCalled() 
		{
			var layout = new CustomLayout();
			var button = new Button();

			Assert.AreEqual(0, layout.AddedCount);
			layout.Add(button);
			Assert.AreEqual(1, layout.AddedCount);
		}

		[Test]
		public void OnChildRemovedCalled()
		{
			var layout = new CustomLayout();
			var button = new Button();
			layout.Add(button);

			Assert.AreEqual(0, layout.RemovedCount);
			layout.Remove(button);
			Assert.AreEqual(1, layout.RemovedCount);
		}
	}
}
