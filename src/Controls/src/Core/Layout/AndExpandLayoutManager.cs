﻿using Microsoft.Maui.Graphics;
using Microsoft.Maui.Layouts;

namespace Microsoft.Maui.Controls
{
	public class AndExpandLayoutManager : ILayoutManager
	{
		IGridLayout _gridLayout;
		readonly StackLayout _stackLayout;
		GridLayoutManager _manager;

		public AndExpandLayoutManager(StackLayout stackLayout)
		{
			_stackLayout = stackLayout;
		}

		public Size Measure(double widthConstraint, double heightConstraint)
		{
			// We have to rebuild this every time because the StackLayout contents
			// and values may have changed
			_gridLayout = Gridify(_stackLayout);
			_manager = new GridLayoutManager(_gridLayout);

			return _manager.Measure(widthConstraint, heightConstraint);
		}

		public Size ArrangeChildren(Rectangle bounds)
		{
			return _manager.ArrangeChildren(bounds);
		}

		IGridLayout Gridify(StackLayout stackLayout)
		{
			if (stackLayout.Orientation == StackOrientation.Vertical)
			{
				return ConvertToRows(stackLayout);
			}

			return ConvertToColumns(stackLayout);
		}

		IGridLayout ConvertToRows(StackLayout stackLayout)
		{
			GridLayout grid = new GridLayout
			{
				ColumnDefinitions = new ColumnDefinitionCollection { new ColumnDefinition { Width = GridLength.Star } },
				RowDefinitions = new RowDefinitionCollection()
			};

			for (int n = 0; n < stackLayout.Count; n++)
			{
				var child = stackLayout[n];

				if (child is View view && view.VerticalOptions.Expands)
				{
					grid.RowDefinitions.Add(new RowDefinition { Height = GridLength.Star });
				}
				else
				{
					grid.RowDefinitions.Add(new RowDefinition { Height = GridLength.Auto });
				}

				grid.Add(child);
				grid.SetRow(child, n);
			}

			return grid;
		}

		IGridLayout ConvertToColumns(StackLayout stackLayout)
		{
			GridLayout grid = new GridLayout
			{
				RowDefinitions = new RowDefinitionCollection { new RowDefinition { Height = GridLength.Star } },
				ColumnDefinitions = new ColumnDefinitionCollection()
			};

			for (int n = 0; n < stackLayout.Count; n++)
			{
				var child = stackLayout[n];

				if (child is View view && view.HorizontalOptions.Expands)
				{
					grid.ColumnDefinitions.Add(new ColumnDefinition { Width = GridLength.Star });
				}
				else
				{
					grid.ColumnDefinitions.Add(new ColumnDefinition { Width = GridLength.Auto });
				}

				grid.Add(child);
				grid.SetColumn(child, n);
			}

			return grid;
		}
	}
}
