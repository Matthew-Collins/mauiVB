using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading;
using System.Xml;

using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.CodeAnalysis.Text;

using Microsoft.Maui.Controls.Xaml;

namespace Microsoft.Maui.Controls.SourceGen.VisualBasic
{
	[Generator(LanguageNames.VisualBasic)]
	public class CodeBehindGenerator : IIncrementalGenerator
	{
		// Source: https://github.com/dotnet/roslyn/blob/main/src/ExpressionEvaluator/Core/Source/FunctionResolver/VisualBasic/Keywords.cs
		static string[] _VisualBasicKeywords = new[] {
					"AddressOf",
					"AddHandler",
					"Alias",
					"And",
					"AndAlso",
					"As",
					"Boolean",
					"ByRef",
					"Byte",
					"ByVal",
					"Call",
					"Case",
					"Catch",
					"CBool",
					"CByte",
					"CChar",
					"CDate",
					"CDec",
					"CDbl",
					"Char",
					"CInt",
					"Class",
					"CLng",
					"CObj",
					"Const",
					"Continue",
					"CSByte",
					"CShort",
					"CSng",
					"CStr",
					"CType",
					"CUInt",
					"CULng",
					"CUShort",
					"Date",
					"Decimal",
					"Declare",
					"Default",
					"Delegate",
					"Dim",
					"DirectCast",
					"Do",
					"Double",
					"Each",
					"Else",
					"ElseIf",
					"End",
					"Enum",
					"Erase",
					"Error",
					"Event",
					"Exit",
					"False",
					"Finally",
					"For",
					"Friend",
					"Function",
					"Get",
					"GetType",
					"GetXmlNamespace",
					"Global",
					"GoTo",
					"Handles",
					"If",
					"Implements",
					"Imports",
					"In",
					"Inherits",
					"Integer",
					"Interface",
					"Is",
					"IsNot",
					"Let",
					"Lib",
					"Like",
					"Long",
					"Loop",
					"Me",
					"Mod",
					"Module",
					"MustInherit",
					"MustOverride",
					"MyBase",
					"MyClass",
					"NameOf",
					"Namespace",
					"Narrowing",
					"Next",
					"New",
					"Not",
					"Nothing",
					"NotInheritable",
					"NotOverridable",
					"Object",
					"Of",
					"On",
					"Operator",
					"Option",
					"Optional",
					"Or",
					"OrElse",
					"Overloads",
					"Overridable",
					"Overrides",
					"ParamArray",
					"Partial",
					"Private",
					"Property",
					"Protected",
					"Public",
					"RaiseEvent",
					"ReadOnly",
					"ReDim",
					"REM",
					"RemoveHandler",
					"Resume",
					"Return",
					"SByte",
					"Select",
					"Set",
					"Shadows",
					"Shared",
					"Short",
					"Single",
					"Static",
					"Step",
					"Stop",
					"String",
					"Structure",
					"Sub",
					"SyncLock",
					"Then",
					"Throw",
					"To",
					"True",
					"Try",
					"TryCast",
					"TypeOf",
					"UInteger",
					"ULong",
					"UShort",
					"Using",
					"When",
					"While",
					"Widening",
					"With",
					"WithEvents",
					"WriteOnly",
					"Xor",
					"EndIf",
					"Gosub",
					"Variant",
					"Wend",
		};

		public void Initialize(IncrementalGeneratorInitializationContext initContext)
		{
			//#if DEBUG
			//			if (!System.Diagnostics.Debugger.IsAttached)
			//				System.Diagnostics.Debugger.Launch();
			//#endif
			var projectItemProvider = initContext.AdditionalTextsProvider
				.Combine(initContext.AnalyzerConfigOptionsProvider)
				.Select(ComputeProjectItem);

			var xmlnsDefinitionsProvider = initContext.CompilationProvider
				.Select(GetXmlnsDefinitionAttributes);

			var sourceProvider = projectItemProvider
				.Combine(xmlnsDefinitionsProvider)
				.Combine(initContext.CompilationProvider)
				.Select(static (t, _) => (t.Left.Left, t.Left.Right, t.Right));

			initContext.RegisterSourceOutput(sourceProvider, static (sourceProductionContext, provider) =>
			{
				var (projectItem, xmlnsDefinitions, compilation) = provider;
				if (projectItem == null)
					return;
				switch (projectItem.Kind)
				{
					case "Xaml":
						GenerateXamlCodeBehind(projectItem, compilation, sourceProductionContext, xmlnsDefinitions);
						break;
					case "Css":
						GenerateCssCodeBehind(projectItem, sourceProductionContext);
						break;
				}
			});
		}

		static ProjectItem? ComputeProjectItem((AdditionalText, AnalyzerConfigOptionsProvider) tuple, CancellationToken cancellationToken)
		{
			var (additionalText, globalOptions) = tuple;
			var options = globalOptions.GetOptions(additionalText);
			if (!options.TryGetValue("build_metadata.additionalfiles.GenKind", out string? kind) || kind is null)
				return null;
			options.TryGetValue("build_metadata.additionalfiles.TargetPath", out var targetPath);
			options.TryGetValue("build_metadata.additionalfiles.ManifestResourceName", out var manifestResourceName);
			options.TryGetValue("build_metadata.additionalfiles.RelativePath", out var relativePath);
			return new ProjectItem(additionalText, targetPath: targetPath, relativePath: relativePath, manifestResourceName: manifestResourceName, kind: kind);
		}

		static IList<XmlnsDefinitionAttribute> GetXmlnsDefinitionAttributes(Compilation compilation, CancellationToken cancellationToken)
		{
			var cache = new List<XmlnsDefinitionAttribute>();
			INamedTypeSymbol? xmlnsDefinitonAttribute = compilation.GetTypeByMetadataName(typeof(XmlnsDefinitionAttribute).FullName);
			if (xmlnsDefinitonAttribute == null)
				return cache;
			foreach (var reference in compilation.References)
			{
				if (compilation.GetAssemblyOrModuleSymbol(reference) is not IAssemblySymbol symbol)
					continue;
				foreach (var attr in symbol.GetAttributes())
				{
					if (!SymbolEqualityComparer.Default.Equals(attr.AttributeClass, xmlnsDefinitonAttribute))
						continue;
					var xmlnsDef = new XmlnsDefinitionAttribute(attr.ConstructorArguments[0].Value as string, attr.ConstructorArguments[1].Value as string);
					if (attr.NamedArguments.Length == 1 && attr.NamedArguments[0].Key == nameof(XmlnsDefinitionAttribute.AssemblyName))
						xmlnsDef.AssemblyName = attr.NamedArguments[0].Value.Value as string;
					else
						xmlnsDef.AssemblyName = symbol.Name;
					cache.Add(xmlnsDef);
				}
			}
			return cache;
		}

		static void GenerateXamlCodeBehind(ProjectItem projItem, Compilation compilation, SourceProductionContext context, IList<XmlnsDefinitionAttribute> xmlnsDefinitionCache)
		{
			using (var reader = File.OpenText(projItem.AdditionalText.Path))
			{
				if (!TryParseXaml(reader, compilation, xmlnsDefinitionCache, out var rootType, out var rootClrNamespace, out var generateDefaultCtor, out var addXamlCompilationAttribute, out var hideFromIntellisense, out var XamlResourceIdOnly, out var baseType, out var namedFields))
					return;

				var sb = new StringBuilder();
				var hintName = $"{(string.IsNullOrEmpty(Path.GetDirectoryName(projItem.TargetPath)) ? "" : Path.GetDirectoryName(projItem.TargetPath) + Path.DirectorySeparatorChar)}{Path.GetFileNameWithoutExtension(projItem.TargetPath)}.{projItem.Kind.ToLowerInvariant()}.sg.cs".Replace(Path.DirectorySeparatorChar, '_');

				if (projItem.ManifestResourceName != null && projItem.TargetPath != null)
					sb.AppendLine($"[assembly: global::Microsoft.Maui.Controls.Xaml.XamlResourceId(\"{projItem.ManifestResourceName}\", \"{projItem.TargetPath.Replace('\\', '/')}\", {(rootType == null ? "null" : "typeof(global::" + rootClrNamespace + "." + rootType + ")")})]");

				if (XamlResourceIdOnly)
				{
					context.AddSource(hintName, SourceText.From(sb.ToString(), Encoding.UTF8));
					return;
				}

				if (rootType == null)
					throw new Exception("Something went wrong");

				sb.AppendLine($"Namespace {rootClrNamespace}");

				sb.AppendLine($"\t[global::Microsoft.Maui.Controls.Xaml.XamlFilePath(\"{projItem.RelativePath?.Replace("\\", "\\\\")}\")]");
				if (addXamlCompilationAttribute)
					sb.AppendLine($"\t<Global.Microsoft.Maui.Controls.Xaml.XamlCompilation(Global.Microsoft.Maui.Controls.Xaml.XamlCompilationOptions.Compile)>");
				if (hideFromIntellisense)
					sb.AppendLine($"\t<Global.System.ComponentModel.EditorBrowsable(Global.System.ComponentModel.EditorBrowsableState.Never)>");

				sb.AppendLine($"\tPublic Partial Class {rootType}");
				sb.AppendLine($"\t\tInherits {baseType}");

				//optional default ctor
				if (generateDefaultCtor)
				{
					sb.AppendLine($"\t\t<Global.System.CodeDom.Compiler.GeneratedCode(\"Microsoft.Maui.Controls.SourceGen\", \"1.0.0.0\")>");
					sb.AppendLine("\t\tPublic Sub New()");
					sb.AppendLine("\t\t\tInitializeComponent()");
					sb.AppendLine("\t\tEnd Sub");
					sb.AppendLine();
				}

				//create fields
				if (namedFields != null)
					foreach ((var fname, var ftype, var faccess) in namedFields)
					{
						sb.AppendLine($"\t\t<Global.System.CodeDom.Compiler.GeneratedCode(\"Microsoft.Maui.Controls.SourceGen\", \"1.0.0.0\")>");

						sb.AppendLine($"\t\t{faccess} WithEvents {(_VisualBasicKeywords.Contains(fname) ? "[" + fname + "]" : fname)} As {ftype}");
						sb.AppendLine();
					}

				//initializeComponent
				sb.AppendLine($"\t\t<Global.System.CodeDom.Compiler.GeneratedCode(\"Microsoft.Maui.Controls.SourceGen\", \"1.0.0.0\")>");
				sb.AppendLine("\t\tPrivate Sub InitializeComponent()");
				sb.AppendLine($"\t\t\tGlobal.Microsoft.Maui.Controls.Xaml.Extensions.LoadFromXaml(Me, GetType({rootType}))");
				if (namedFields != null)
					foreach ((var fname, var ftype, var faccess) in namedFields)
						sb.AppendLine($"\t\t\t{(_VisualBasicKeywords.Contains(fname) ? "@" + fname : fname)} = Global.Microsoft.Maui.Controls.NameScopeExtensions.FindByName(Of {ftype})(Me, \"{fname}\")");

				sb.AppendLine("\t\tEnd Sub");
				sb.AppendLine("\tEnd Class");
				sb.AppendLine("End Namespace");

				context.AddSource(hintName, SourceText.From(sb.ToString(), Encoding.UTF8));
			}
		}

		static bool TryParseXaml(TextReader xaml, Compilation compilation, IList<XmlnsDefinitionAttribute> xmlnsDefinitionCache, out string? rootType, out string? rootClrNamespace, out bool generateDefaultCtor, out bool addXamlCompilationAttribute, out bool hideFromIntellisense, out bool xamlResourceIdOnly, out string? baseType, out IEnumerable<(string, string, string)>? namedFields)
		{
			rootType = null;
			rootClrNamespace = null;
			generateDefaultCtor = false;
			addXamlCompilationAttribute = false;
			hideFromIntellisense = false;
			xamlResourceIdOnly = false;
			namedFields = null;
			baseType = null;

			var xmlDoc = new XmlDocument();
			xmlDoc.Load(xaml);

			// if the following xml processing instruction is present
			//
			// <?xaml-comp compile="true" ?>
			//
			// we will generate a xaml.g.vb file with the default ctor calling InitializeComponent, and a XamlCompilation attribute
			var hasXamlCompilationProcessingInstruction = GetXamlCompilationProcessingInstruction(xmlDoc);

			var nsmgr = new XmlNamespaceManager(xmlDoc.NameTable);
			nsmgr.AddNamespace("__f__", XamlParser.MauiUri);

			var root = xmlDoc.SelectSingleNode("/*", nsmgr);
			if (root == null)
				return false;

			foreach (XmlAttribute attr in root.Attributes)
			{
				if (attr.Name == "xmlns")
					nsmgr.AddNamespace("", attr.Value); //Add default xmlns
				if (attr.Prefix != "xmlns")
					continue;
				nsmgr.AddNamespace(attr.LocalName, attr.Value);
			}

			var rootClass = root.Attributes["Class", XamlParser.X2006Uri]
						 ?? root.Attributes["Class", XamlParser.X2009Uri];

			if (rootClass != null)
				XmlnsHelper.ParseXmlns(rootClass.Value, out rootType, out rootClrNamespace, out var rootAsm, out var targetPlatform);
			else if (hasXamlCompilationProcessingInstruction)
			{
				rootClrNamespace = "__XamlGeneratedCode__";
				rootType = $"__Type{Guid.NewGuid():N}";
				generateDefaultCtor = true;
				addXamlCompilationAttribute = true;
				hideFromIntellisense = true;
			}
			else
			{ // rootClass == null && !hasXamlCompilationProcessingInstruction) {
				xamlResourceIdOnly = true; //only generate the XamlResourceId assembly attribute
				return true;
			}

			namedFields = GetNamedFields(root, nsmgr, compilation, xmlnsDefinitionCache);
			var typeArguments = GetAttributeValue(root, "TypeArguments", XamlParser.X2006Uri, XamlParser.X2009Uri);
			baseType = GetTypeName(new XmlType(root.NamespaceURI, root.LocalName, typeArguments != null ? TypeArgumentsParser.ParseExpression(typeArguments, nsmgr, null) : null), compilation, xmlnsDefinitionCache);

			return true;
		}


		static bool GetXamlCompilationProcessingInstruction(XmlDocument xmlDoc)
		{
			var instruction = xmlDoc.SelectSingleNode("processing-instruction('xaml-comp')") as XmlProcessingInstruction;
			if (instruction == null)
				return false;

			var parts = instruction.Data.Split(' ', '=');
			var indexOfCompile = Array.IndexOf(parts, "compile");
			if (indexOfCompile != -1)
				return parts[indexOfCompile + 1].Trim('"', '\'').Equals("true", StringComparison.InvariantCultureIgnoreCase);
			return false;
		}

		static IEnumerable<(string name, string type, string accessModifier)> GetNamedFields(XmlNode root, XmlNamespaceManager nsmgr, Compilation compilation, IList<XmlnsDefinitionAttribute> xmlnsDefinitionCache)
		{
			var xPrefix = nsmgr.LookupPrefix(XamlParser.X2006Uri) ?? nsmgr.LookupPrefix(XamlParser.X2009Uri);
			if (xPrefix == null)
				yield break;

			XmlNodeList names =
				root.SelectNodes(
					"//*[@" + xPrefix + ":Name" +
					"][not(ancestor:: __f__:DataTemplate) and not(ancestor:: __f__:ControlTemplate) and not(ancestor:: __f__:Style) and not(ancestor:: __f__:VisualStateManager.VisualStateGroups)]", nsmgr);
			foreach (XmlNode node in names)
			{
				var name = GetAttributeValue(node, "Name", XamlParser.X2006Uri, XamlParser.X2009Uri) ?? throw new Exception();
				var typeArguments = GetAttributeValue(node, "TypeArguments", XamlParser.X2006Uri, XamlParser.X2009Uri);
				var fieldModifier = GetAttributeValue(node, "FieldModifier", XamlParser.X2006Uri, XamlParser.X2009Uri);

				var xmlType = new XmlType(node.NamespaceURI, node.LocalName,
										  typeArguments != null
										  ? TypeArgumentsParser.ParseExpression(typeArguments, nsmgr, null)
										  : null);

				var accessModifier = fieldModifier?.ToLowerInvariant().Replace("notpublic", "internal") ?? "private"; //notpublic is WPF for internal
				if (!new[] { "private", "public", "internal", "protected" }.Contains(accessModifier)) //quick validation
					accessModifier = "private";
				yield return (name ?? "", GetTypeName(xmlType, compilation, xmlnsDefinitionCache), accessModifier);
			}
		}

		static string GetTypeName(XmlType xmlType, Compilation compilation, IList<XmlnsDefinitionAttribute> xmlnsDefinitionCache)
		{
			string returnType;
			var ns = GetClrNamespace(xmlType.NamespaceUri);
			if (ns != null)
				returnType = $"{ns}.{xmlType.Name}";
			else
			{
				// It's an external, non-built-in namespace URL.
				returnType = GetTypeNameFromCustomNamespace(xmlType, compilation, xmlnsDefinitionCache);
			}

			if (xmlType.TypeArguments != null)
				returnType = $"{returnType}<{string.Join(", ", xmlType.TypeArguments.Select(typeArg => GetTypeName(typeArg, compilation, xmlnsDefinitionCache)))}>";

			return $"Global.{returnType}";
		}

		static string? GetClrNamespace(string namespaceuri)
		{
			if (namespaceuri == XamlParser.X2009Uri)
				return "System";
			if (namespaceuri != XamlParser.X2006Uri &&
				!namespaceuri.StartsWith("clr-namespace", StringComparison.InvariantCulture) &&
				!namespaceuri.StartsWith("using:", StringComparison.InvariantCulture))
				return null;
			return XmlnsHelper.ParseNamespaceFromXmlns(namespaceuri);
		}

		static string GetTypeNameFromCustomNamespace(XmlType xmlType, Compilation compilation, IList<XmlnsDefinitionAttribute> xmlnsDefinitionCache)
		{
#nullable disable
			string typeName = xmlType.GetTypeReference<string>(xmlnsDefinitionCache, null,
				(typeInfo) =>
				{
					string typeName = typeInfo.typeName.Replace('+', '/'); //Nested types
					string fullName = $"{typeInfo.clrNamespace}.{typeInfo.typeName}";

					if (compilation.GetTypeByMetadataName(fullName) != null)
						return fullName;
					return null;
				});

			return typeName;
#nullable enable
		}

		static string? GetAttributeValue(XmlNode node, string localName, params string[] namespaceURIs)
		{
			if (node == null)
				throw new ArgumentNullException(nameof(node));
			if (localName == null)
				throw new ArgumentNullException(nameof(localName));
			if (namespaceURIs == null)
				throw new ArgumentNullException(nameof(namespaceURIs));
			foreach (var namespaceURI in namespaceURIs)
			{
				var attr = node.Attributes[localName, namespaceURI];
				if (attr == null)
					continue;
				return attr.Value;
			}
			return null;
		}

		static void GenerateCssCodeBehind(ProjectItem projItem, SourceProductionContext sourceProductionContext)
		{
			var sb = new StringBuilder();
			var hintName = $"{(string.IsNullOrEmpty(Path.GetDirectoryName(projItem.TargetPath)) ? "" : Path.GetDirectoryName(projItem.TargetPath) + Path.DirectorySeparatorChar)}{Path.GetFileNameWithoutExtension(projItem.TargetPath)}.{projItem.Kind.ToLowerInvariant()}.sg.cs".Replace(Path.DirectorySeparatorChar, '_');

			if (projItem.ManifestResourceName != null && projItem.TargetPath != null)
				sb.AppendLine($"[assembly: global::Microsoft.Maui.Controls.Xaml.XamlResourceId(\"{projItem.ManifestResourceName}\", \"{projItem.TargetPath.Replace('\\', '/')}\", null)]");

			sourceProductionContext.AddSource(hintName, SourceText.From(sb.ToString(), Encoding.UTF8));

		}

		class ProjectItem
		{
			public ProjectItem(AdditionalText additionalText, string? targetPath, string? relativePath, string? manifestResourceName, string kind)
			{
				AdditionalText = additionalText;
				TargetPath = targetPath ?? additionalText.Path;
				RelativePath = relativePath;
				ManifestResourceName = manifestResourceName;
				Kind = kind;
			}

			public AdditionalText AdditionalText { get; }
			public string? TargetPath { get; }
			public string? RelativePath { get; }
			public string? ManifestResourceName { get; }
			public string Kind { get; }
		}
	}
}
