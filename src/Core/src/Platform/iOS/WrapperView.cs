﻿using CoreAnimation;
using CoreGraphics;
using Microsoft.Maui.Graphics;
using Microsoft.Maui.Graphics.Native;
using ObjCRuntime;
using UIKit;

namespace Microsoft.Maui
{
	public partial class WrapperView : UIView
	{
		CAShapeLayer? _maskLayer;
		CAShapeLayer? _shadowLayer;

		public WrapperView()
		{
		}

		public WrapperView(CGRect frame)
			: base(frame)
		{
		}

		CAShapeLayer? MaskLayer
		{
			get => _maskLayer;
			set
			{
				var layer = GetLayer();

				if (layer != null && _maskLayer != null)
					layer.Mask = null;

				_maskLayer = value;

				if (layer != null)
					layer.Mask = value;
			}
		}

		CAShapeLayer? ShadowLayer
		{
			get => _shadowLayer;
			set
			{
				_shadowLayer?.RemoveFromSuperLayer();
				_shadowLayer = value;

				if (_shadowLayer != null)
					Layer.InsertSublayer(_shadowLayer, 0);
			}
		}

		public override void LayoutSubviews()
		{
			base.LayoutSubviews();

			if (Subviews.Length == 0)
				return;

			var child = Subviews[0];

			child.Frame = Bounds;

			if (MaskLayer != null)
				MaskLayer.Frame = Bounds;

			if (ShadowLayer != null)
				ShadowLayer.Frame = Bounds;

			SetClip();
			SetShadow();
		}

		public override CGSize SizeThatFits(CGSize size)
		{
			if (Subviews.Length == 0)
				return base.SizeThatFits(size);

			var child = Subviews[0];

			return child.SizeThatFits(size);
		}

		public override void SetNeedsLayout()
		{
			base.SetNeedsLayout();

			Superview?.SetNeedsLayout();
		}

		partial void ClipChanged()
		{
			SetClip();
		}

		partial void ShadowChanged()
		{
			SetShadow();
		}

		void SetClip()
		{
			var mask = MaskLayer;

			if (mask == null && Clip == null)
				return;

			mask ??= MaskLayer = new CAShapeLayer();
			var frame = Frame;
			var bounds = new RectangleF(0, 0, (float)frame.Width, (float)frame.Height);

			var path = _clip?.PathForBounds(bounds);
			var nativePath = path?.AsCGPath();
			mask.Path = nativePath;
		}

		void SetShadow()
		{
			var shadowLayer = ShadowLayer;

			if (shadowLayer == null && Shadow == null)
				return;

			shadowLayer ??= ShadowLayer = new CAShapeLayer();

			var frame = Frame;
			var bounds = new RectangleF(0, 0, (float)frame.Width, (float)frame.Height);

			shadowLayer.FillColor = new CGColor(0, 0, 0, 1);

			var path = _clip?.PathForBounds(bounds);
			var nativePath = path?.AsCGPath();
			shadowLayer.Path = nativePath;

			if (Shadow == null)
				shadowLayer.ClearShadow();
			else
				shadowLayer.SetShadow(Shadow);
		}

		CALayer? GetLayer()
		{
			if (Layer == null || Layer.Sublayers == null)
				return null;

			foreach (var subLayer in Layer.Sublayers)
				if (subLayer.Delegate != null)
					return subLayer;

			return Layer;
		}
	}
}