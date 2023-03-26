//
//  ContentView.swift
//  Smol
//
//  Created by Jason Brennan on 2/9/23.
//

import SwiftUI

struct BrowserView: View {
	@ObservedObject var controller: PageController
	@FocusState private var addressIsFocused: Bool
	
	var body: some View {
		VStack(spacing: 0) {
			HStack {
				HStack(spacing: 0) {
					Button(action: { controller.goBack() }) {
						Image(systemName: "arrowtriangle.left.fill")
					}.disabled(controller.canGoBack == false)
					Button(action: { controller.goForward() }) {
						Image(systemName: "arrowtriangle.right.fill")
					}.disabled(controller.canGoForward == false)
				}
				TextField("Address", text: $controller.address)
					.onSubmit {
						addressIsFocused = false
						guard let url = URL(string: controller.address) else { return }
						controller.loadPage(at: fullURL(forURLToLoad: url))
					}
					.textFieldStyle(RoundedBorderTextFieldStyle())
					.focused($addressIsFocused)
			}
			.padding()
			Divider()
			WebDocumentView(controller: controller)
				.background(.white)
				.environment(\.openURL, .init(handler: { url in
					if let scheme = url.scheme, (scheme == "http" || scheme == "https") == false {
						return .systemAction
					}
					controller.loadPage(at: fullURL(forURLToLoad: url))
					addressIsFocused = false
					return .handled
				}))
		}
		.environment(\.urlBuilder, fullURL(forURLToLoad:))
	}
	
	private func fullURL(forURLToLoad urlToLoad: URL) -> URL {
		if urlToLoad.host != nil { return urlToLoad }
		
		switch controller.state {
		case .failed, .notLoaded: return urlToLoad
		case .loaded(_, let loadedURL):
			return URL(string: urlToLoad.path, relativeTo: loadedURL.deletingLastPathComponent()) ?? urlToLoad
		}
	}
}

struct WebDocumentView: View {
	@ObservedObject var controller: PageController
	
	var body: some View {
		switch controller.state {
		case .notLoaded:
			Text("Let's load a web page!")
				.frame(maxWidth: .infinity, maxHeight: .infinity)
		case .failed(let error):
			Text(verbatim: "Failed to load page. Error: \(error)")
				.frame(maxWidth: .infinity, maxHeight: .infinity)
		case .loaded(let document, _):
			BodyView(bodyNode: document.htmlNode.firstDirectChild(named: "body")!)
				.navigationTitle(
					document
						.htmlNode
						.firstDirectChild(named: "head")?
						.firstDirectChild(named: "title")?
						.firstDirectChild(named: Node.InternalElement.textRun)?
						.textContent ?? "Smol"
				)
				.environment(\.font, Font.custom("Times", size: 16))
		}
	}
}

private struct URLBuilderKey: EnvironmentKey {
	static let defaultValue: (URL) -> URL = { $0 }
}

extension EnvironmentValues {
	/// A function that takes a (potentially "relative") web url to load, and fleshes it out to a full url that includes a host.
	var urlBuilder: (URL) -> URL {
		get { self[URLBuilderKey.self] }
		set { self[URLBuilderKey.self] = newValue }
	}
}

class PageController: ObservableObject {
	
	enum State {
		case notLoaded
		case loaded(Document, URL)
		case failed(Error)
	}
	
	private enum LoadingError: Error {
		case failedToLoad(URL)
	}
	
	@Published var state = State.notLoaded {
		didSet {
			if let currentlyLoadedDocument {
				address = currentlyLoadedDocument.1.absoluteString
			}
		}
	}
	var address = "https://nearthespeedoflight.com/smol.html"
	
	private var backStack: [(Document, URL)] = []
	private var forwardStack: [(Document, URL)] = []
	
	var canGoBack: Bool { backStack.isEmpty == false }
	var canGoForward: Bool { forwardStack.isEmpty == false }
	
	func loadPage(at url: URL) {
		Task {
			let (data, response) = try await URLSession.shared.data(from: url)
			let htmlString = String(data: data, encoding: .utf8) ?? ""
			let tokenizer = Tokenizer(programText: htmlString)
			let context = try ParsingContext(tokens: tokenizer.scanAllTokens())
			
			await MainActor.run {
				do {
					let oldCurrentlyLoadedDocument = currentlyLoadedDocument
					state = .loaded(try Document.parse(context: context, options: nil), response.url ?? url)
					if let oldCurrentlyLoadedDocument {
						backStack.append(oldCurrentlyLoadedDocument)
						forwardStack = []
					}
				} catch {
					print("error parsing document: \(error)")
					state = .failed(error)
				}
			}
		}
	}
	
	private var currentlyLoadedDocument: (Document, URL)? {
		switch state {
		case .notLoaded, .failed: return nil
		case let .loaded(document, url): return (document, url)
		}
	}
	
	func goBack() {
		guard let currentlyLoadedDocument else { return }
		guard let (previousDocument, previousURL) = backStack.popLast() else { return }
		state = .loaded(previousDocument, previousURL)
		forwardStack.append(currentlyLoadedDocument)
	}
	
	func goForward() {
		guard let currentlyLoadedDocument else { return }
		guard let (nextDocument, nextURL) = forwardStack.popLast() else { return }
		state = .loaded(nextDocument, nextURL)
		backStack.append(currentlyLoadedDocument)
	}
}

let pageController = PageController()

struct BrowserView_Previews: PreviewProvider {
    static var previews: some View {
		BrowserView(controller: pageController)
    }
}

/// This view works as a generic "block" / "box" container for inline content.
///
/// You might use it for paragraph contents, or h1/2/3/etc contents, or just inline content not in one of those elements.
struct InlineContentWrappingBlockView: View {
	let node: Node
	@Environment(\.font) var font
	
	var body: some View {
		Text(
			node
				.childNodes
				.map { $0.attributedText(defaultFont: font ?? Font.custom("Times", size: 16)) }
				.reduce(AttributedString(), +)
		).fixedSize(horizontal: false, vertical: true)
	}
}


struct ImageView: View {
	let node: Node
	@Environment(\.urlBuilder) var urlBuilder
	
	var body: some View {
		AsyncImage(url: urlBuilder(URL.init(string: node.attributeDictionary["src"] ?? "")!), content: { image in
			image
				.resizable()
				.aspectRatio(contentMode: .fit)
				.frame(
					width: node.attributeDictionary["width"].flatMap(WebSize.init(rawValue:))?.dimension,
					height: node.attributeDictionary["height"].flatMap(WebSize.init(rawValue:))?.dimension
				)
		}, placeholder: {
			Color(white: 0.9).cornerRadius(4)
		})
	}
}

struct WebSize {
	let rawValue: String
	
	var dimension: CGFloat {
		// trim anything that isn't a digit, then try to parse that into an int. this ignores things like "px"
		CGFloat(Int(rawValue.prefix(while: \.isWholeNumber)) ?? 0)
	}
}

struct BlocksView: View {
	let children: [Node]
	@Environment(\.font) var font
	
	var body: some View {
		VStack(alignment: .leading, spacing: 20) {
			ForEach(children, id: \.self) { childNode in
				switch childNode.element {
				case "h1":
					InlineContentWrappingBlockView(node: childNode)
						.font(Font.custom("Times", size: 32).bold())
				case "h2":
					InlineContentWrappingBlockView(node: childNode)
						.font(Font.custom("Times", size: 28).bold())
				case "h3":
					InlineContentWrappingBlockView(node: childNode)
						.font(Font.custom("Times", size: 24).bold())
				case "p":
					InlineContentWrappingBlockView(node: childNode)
				case "img":
					ImageView(node: childNode)
				case "div", "section", "main", "footer", "article", "header", "nav", "aside":
					BlocksView(children: childNode.childNodesSortedIntoBlocks)
				case "pre":
					BlocksView(children: childNode.childNodesSortedIntoBlocks)
						.font(font?.monospaced())
				case "blockquote":
					BlocksView(children: childNode.childNodesSortedIntoBlocks)
						.padding(.leading, 20)
				case "ul": ListNodeView(node: childNode, style: .unordered)
				case "ol": ListNodeView(node: childNode, style: .ordered)
				case "hr": Divider()
				case "script": EmptyView()
				case "br": Color.clear.padding(20)
				default: Text("unknown block element: <\(childNode.element)>")
				}
			}
		}
	}
}

struct ListNodeView: View {
	enum Style {
		case ordered, unordered
		
		func listMarker(for index: Int) -> String {
			switch self {
			case .ordered: return "\(index + 1)."
			case .unordered: return "•"
			}
		}
	}
	
	let node: Node
	let style: Style
	
	var body: some View {
		VStack(alignment: .leading, spacing: 0) {
			ForEach(Array(zip(node.childNodes.indices, node.childNodes)), id: \.1) { (index, childNode) in
				HStack(alignment: .top, spacing: 8) {
					Text(verbatim: style.listMarker(for: index))
					BlocksView(children: childNode.childNodesSortedIntoBlocks)
				}
			}
		}
	}
}

struct BodyView: View {
	let bodyNode: Node
	var body: some View {
		ScrollView {
			HStack(spacing: 0) {
				BlocksView(children: bodyNode.childNodesSortedIntoBlocks)
					.font(Font.custom("Times", size: 16))
					.frame(maxWidth: bodyNode.styleFromAttributes?.maxWidth)
				Spacer()
			}
			.padding(20)
		}
		.frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .topLeading)
		.background(Color.white)
	}
}

extension Node {
	func attributedText(defaultFont: Font) -> AttributedString {
		switch element {
		case InternalElement.textRun:
			var attributes = AttributeContainer()
			attributes.font = defaultFont
			
			return AttributedString(textContent ?? "", attributes: attributes)
		case "em", "i":
			var attributes = AttributeContainer()
			attributes.font = defaultFont.italic()
			
			return childNodes
				.map { $0.attributedText(defaultFont: defaultFont.italic()) }
				.reduce(AttributedString(), +)
				.mergingAttributes(attributes, mergePolicy: .keepCurrent)
		case "strong", "b":
			var attributes = AttributeContainer()
			attributes.font = defaultFont.bold()
			
			return childNodes
				.map { $0.attributedText(defaultFont: defaultFont.bold()) }
				.reduce(AttributedString(), +)
				.mergingAttributes(attributes, mergePolicy: .keepCurrent)
		case "code":
			var attributes = AttributeContainer()
			attributes.font = defaultFont.monospaced()
			
			return childNodes
				.map { $0.attributedText(defaultFont: defaultFont.monospaced()) }
				.reduce(AttributedString(), +)
				.mergingAttributes(attributes, mergePolicy: .keepCurrent)
		case "a":
			var attributes = AttributeContainer()
			attributes.link = URL(string: attributeDictionary["href"] ?? "")
			attributes.underlineStyle = .single
			
			return childNodes
				.map { $0.attributedText(defaultFont: defaultFont) }
				.reduce(AttributedString(), +)
				.mergingAttributes(attributes, mergePolicy: .keepCurrent)
		default:
			return AttributedString("Unknown inline element <\(element)>")
			var attributes = AttributeContainer()
			attributes.font = defaultFont
			
			return childNodes
				.map { $0.attributedText(defaultFont: defaultFont) }
				.reduce(AttributedString(), +)
				.mergingAttributes(attributes, mergePolicy: .keepCurrent)
		}
	}
}

struct Token: Equatable, CustomDebugStringConvertible {
	
	enum Kind: Equatable {
		case text, openAngleBracket, closeAngleBracket, forwardSlash, equals, hyphen, singleQuote, doubleQuote, whitespace, bang
	}
	
	let kind: Kind
	let body: String
	
	init(kind: Kind, body: String) {
		self.kind = kind
		self.body = body
	}
	
	init?(symbol: Character) {
		switch symbol {
		case "<": self.init(kind: .openAngleBracket, body: "<")
		case ">": self.init(kind: .closeAngleBracket, body: ">")
		case "/": self.init(kind: .forwardSlash, body: "/")
		case "=": self.init(kind: .equals, body: "=")
		case "-": self.init(kind: .hyphen, body: "-")
		case "'": self.init(kind: .singleQuote, body: "'")
		case "\"": self.init(kind: .doubleQuote, body: "\"")
		case "!": self.init(kind: .bang, body: "!")
		default: return nil
		}
	}
	
	var debugDescription: String { body }
}

class ScanningCursor {
	private let programText: String
	var currentIndex: String.Index
	
	var isNotAtEnd: Bool { currentIndex < programText.endIndex }
	
	init(programText: String) {
		self.programText = programText
		self.currentIndex = programText.startIndex
	}
	
	@discardableResult
	func advance() -> Character {
		guard isNotAtEnd else { fatalError() }
		
		let currentCharacter = currentCharacter()
		currentIndex = programText.index(after: currentIndex)
		
		return currentCharacter
	}
	
	func currentCharacter() -> Character {
		programText[currentIndex]
	}
	
	func previousCharacter() -> Character {
		programText[programText.index(before: currentIndex)]
	}
}

class Tokenizer {
	private let cursor: ScanningCursor
	var scannedTokens = [Token]()
	
	init(programText: String) {
		cursor = ScanningCursor(programText: programText)
	}
	
	func scanAllTokens() throws -> [Token] {
		while cursor.isNotAtEnd {
			try scanNextToken()
		}
		
		return scannedTokens
	}
	
	private func scanNextToken() throws {
		let next = cursor.advance()
		
		if let token = Token(symbol: next) {
			return scannedTokens.append(token)
		} else if next.isWhitespace {
			return scannedTokens.append(Token(kind: .whitespace, body: String(next)))
		} else {
			scanText()
		}
	}
	
	private func scanText() {
		var body = String(cursor.previousCharacter())
		while cursor.isNotAtEnd {
			let next = cursor.currentCharacter()
			if Token(symbol: next) != nil {
				break
			}
			if next.isWhitespace { break }
			
			body.append(next)
			cursor.advance()
		}
		scannedTokens.append(Token(kind: .text, body: body))
	}
}

class ParsingContext {
	
	private let tokens: [Token]
	private var tokenIndexStack = [0]
	private var currentTokenIndex: Int {
		get { tokenIndexStack.last! }
		set { tokenIndexStack[tokenIndexStack.endIndex - 1] = newValue }
	}
	
	var isNotAtEnd: Bool {
		currentTokenIndex < tokens.count
	}
	
	/// Might be a whitespace token.
	var currentToken: Token { isNotAtEnd == false ? tokens.last! : tokens[currentTokenIndex] }
	
	/// Might be a whitespace token.
	var nextToken: Token { tokens[currentTokenIndex + 1] }
	
	/// Might be a whitespace token.
	var nextNextToken: Token { tokens[currentTokenIndex + 2] }
	var previousToken: Token { tokens[currentTokenIndex - 1] }
	
	init(tokens: [Token]) {
		self.tokens = tokens
	}
	
	enum ParseError: Error {
		case unexpectedToken(Token, feedback: String)
		case failedToParse
	}
	
	@discardableResult
	func consume(tokenKind kind: Token.Kind, feedback: String) throws -> Token {
		try consume(where: { $0.kind == kind }, feedback: feedback)
	}
	
	@discardableResult
	func consume(where predicate: (Token) -> Bool, skipWhitespaceTokens: Bool = true, feedback: String) throws -> Token {
		let oldCurrentToken = self.currentToken
		guard advance(when: predicate, skipWhitespaceTokens: skipWhitespaceTokens) else {
			throw ParseError.unexpectedToken(oldCurrentToken, feedback: feedback)
		}
		return previousToken
	}
	
	/// Similar to `whileNotAtEnd()`, except this call does not `throw`. If the given `perform` closure throws, the results accumulated thus far are returned, vs just propagating up the error like `whileNotAtEnd()` does.
	///
	/// Use this method when you want to accumulate results until parsing fails, but you want to keep what you've found so far.
	func untilThrowOrEndOfTokensReached<ConsumedType>(perform: () throws -> ConsumedType) -> [ConsumedType] {
		untilErrorThrownOrEndOfTokensReached(perform: perform).0
	}
	
	/// Similar to `untilThrowOrEndOfTokensReached` but this one includes the error, if any, that was thrown that ended iterating.
	///
	/// You probably want to use the error-less variant of this method most of the time, but this one is useful if you're trying to debug or want fine grained control.
	func untilErrorThrownOrEndOfTokensReached<ConsumedType>(perform: () throws -> ConsumedType) -> ([ConsumedType], Error?) {
		var results = [ConsumedType]()
		
		do {
			while isNotAtEnd {
				results.append(try perform())
			}
		} catch {
			return (results, error)
		}
		return (results, nil)
	}
	
	func attempt<ContentType>(action: () throws -> ContentType) throws -> ContentType {
		tokenIndexStack.append(currentTokenIndex)
		var shouldRevertIndexStack = true
		
		defer {
			// Pop the stack if `try action()` fails.
			// doing it this way, instead of catching + rethrowing
			// so that the error chain continues to the original error, not our rethrow
			if shouldRevertIndexStack {
				_ = tokenIndexStack.popLast()
			}
		}
		
		let result = try action()
		
		// we succeeded, so pop the token index stack, and use THAT value as the new current index
		currentTokenIndex = tokenIndexStack.popLast()!
		shouldRevertIndexStack = false
		return result
	}
	
	func choose<ContentType>(from choices: [() throws -> ContentType]) throws -> ContentType {
		try attempt(action: {
			var mostRecentError: Error = ParseError.failedToParse
			
			for choice in choices {
				
				do {
					return try attempt(action: {
						try choice()
					})
				} catch {
					mostRecentError = error
				}
			}
			
			throw mostRecentError
		})
	}
	
	private func advance(when predicate: (Token) -> Bool, skipWhitespaceTokens: Bool = true) -> Bool {
		
		var skippedWhitespaceCount = 0
		if skipWhitespaceTokens {
			while isNotAtEnd && currentToken.kind == .whitespace {
				currentTokenIndex += 1
				skippedWhitespaceCount += 1
			}
		}
		
		guard isNotAtEnd else { return false }
		
		if predicate(tokens[currentTokenIndex]) {
			currentTokenIndex += 1
			return true
		} else {
			currentTokenIndex -= skippedWhitespaceCount
			return false
		}
	}
}

protocol Parsable {
	static func parse(context: ParsingContext, options: ParsingOptions?) throws -> Self
}

struct ParsingOptions {
	let preservesWhitespace: Bool
}

/// This type mostly exists right now to handle parsing pages that have a `<!doctype>` node at their root, along with an `<html>` node.
/// For now, we're discarding the doctype.
struct Document: Hashable, Parsable {
	
	enum DocumentError: Error {
		case unableToFindHTMLNode
	}
	
	let htmlNode: Node
	
	static func parse(context: ParsingContext, options: ParsingOptions?) throws -> Document {
		let (nodes, error) = context.untilErrorThrownOrEndOfTokensReached {
			try Node.parse(context: context, options: nil)
		}
		
		if let error {
			print("Document finished parsing with an error: \(error)")
		}
		
		guard let htmlNode = nodes.first(where: { $0.element.lowercased() == "html" }) else {
			throw DocumentError.unableToFindHTMLNode
		}
		
		return Document(htmlNode: htmlNode)
	}
}

struct Node: Hashable, Parsable, Identifiable {
	
	struct InternalElement {
		static let textRun = "__textRun"
		static let comment = "__comment"
	}
	
	enum Content: Hashable {
		case text(String)
		case childNodes([Node])
		case voidNode
	}
	
	let element: String
	let content: Content
	let attributes: [Attribute]
	
	// todo: this id is breaking all the tests
	let id = UUID()
	
	enum NodeParseError: Error {
		case closingTagDidNotMatchOpeningTag(opening: String, closing: String)
		
		/// This error will probably get thrown a lot, just to signify that parsing a child failed.
		/// todo: It's probably wasteful to do it this way!
		case openingTagWasActuallyClosing(tagName: String)
		case closingTagWasActuallyOpening(tagName: String)
		case didNotFindAnyText
	}
	
	static func parse(context: ParsingContext, options: ParsingOptions?) throws -> Node {
		
		let startTag = try Tag.parse(context: context, options: options)
		
		guard startTag.isEnd == false else {
			throw NodeParseError.openingTagWasActuallyClosing(tagName: startTag.element)
		}
		
		
		if startTag.element.lowercased() == "doctype" {
			return Node(element: "doctype", content: .voidNode, attributes: startTag.attributes)
		}
		
		if startTag.isVoidElement {
			return Node(element: startTag.element, content: .voidNode, attributes: startTag.attributes)
		}
		
		let shouldPreserveWhitespace = startTag.element == "pre" || options?.preservesWhitespace ?? false
		
		let children = context.untilThrowOrEndOfTokensReached(perform: {
			try context.choose(from: [
				{ try Node.parse(context: context, options: .init(preservesWhitespace: shouldPreserveWhitespace)) },
				{
					let textContents = context.untilThrowOrEndOfTokensReached {
						try context.consume(where: { $0.kind != .openAngleBracket }, skipWhitespaceTokens: false, feedback: "Expected a non `<` token")
					}
					guard textContents.isEmpty == false else {
						throw NodeParseError.didNotFindAnyText
					}
					
					// todo: this does not follow the exact html rules, but good enough for now
					let contentRun: String
					if shouldPreserveWhitespace {
						contentRun = textContents
							.map(\.body)
							.joined()
							.replacingOccurrences(of: "&#x000A;", with: "")
							.replacingOccurrences(of: "&lt;", with: "<")
							.replacingOccurrences(of: "&gt;", with: ">")
							.replacingOccurrences(of: "&quot;", with: "\"")
							.replacingOccurrences(of: "&amp;", with: "&")
					} else {
						contentRun = textContents
							.map(\.body)
							.joined()
							.replacingOccurrences(of: "&#x000A;", with: "")
							.replacingOccurrences(of: "\n", with: " ")
							.replacingOccurrences(of: "\t", with: " ")
							.replacingOccurrences(of: "&lt;", with: "<")
							.replacingOccurrences(of: "&gt;", with: ">")
							.replacingOccurrences(of: "&quot;", with: "\"")
							.replacingOccurrences(of: "&amp;", with: "&")
					}
					
//					guard contentRun.isEmpty == false && contentRun.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty == false else {
//						throw NodeParseError.didNotFindAnyText
//					}
					
					return Node(element: InternalElement.textRun, content: .text(contentRun), attributes: [])
				},
				{
					try context.consumeBetween(leftToken: .openAngleBracket, rightToken: .closeAngleBracket) {
						try context.consume(tokenKind: .bang, feedback: "Expected comment to begin with a bang")
						try context.consume(tokenKind: .hyphen, feedback: "Expected comment to have a hyphen after the bang")
						try context.consume(tokenKind: .hyphen, feedback: "Expected comment to have two hyphens after the bang")
						
						var done = false
						while done == false {
							
							if context.currentToken.kind == .hyphen && context.nextToken.kind == .hyphen && context.nextNextToken.kind == .closeAngleBracket {
								
								try context.consume(tokenKind: .hyphen, feedback: "-")
								try context.consume(tokenKind: .hyphen, feedback: "-")
								done = true
							} else {
								try context.consume(where: { _ in true }, skipWhitespaceTokens: false, feedback: "munch munch")
							}
						}
						
						return Node(element: InternalElement.comment, content: .voidNode, attributes: [])
					}
				}
			])
		})
			.filter {
				if $0.element == InternalElement.comment { return false }
				if $0.element != InternalElement.textRun { return true }
				
				// filter out empty text run nodes
				return $0.textContent?.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty == false
			}
		
		let endTag = try Tag.parse(context: context, options: options)
		guard endTag.isEnd else {
			throw NodeParseError.closingTagWasActuallyOpening(tagName: endTag.element)
		}
		
		guard startTag.element == endTag.element else {
			throw NodeParseError.closingTagDidNotMatchOpeningTag(opening: startTag.element, closing: endTag.element)
		}
		
		return .init(
			element: startTag.element,
			content: .childNodes(children),
			attributes: startTag.attributes
		)
	}
}

struct Tag: Parsable {
	
	/// A "void element" is one that has no end tag and no children.
	static let voidElements = ["area", "base", "br", "col", "embed", "hr", "img", "input", "link", "meta", "source", "track", "wbr"]
	
	let element: String
	let isEnd: Bool
	let attributes: [Attribute]
	
	var isVoidElement: Bool {
		Tag.voidElements.contains(element)
	}
	
	static func parse(context: ParsingContext, options: ParsingOptions?) throws -> Tag {
		try context.consumeBetween(leftToken: .openAngleBracket, rightToken: .closeAngleBracket) {
			let slashToken = try? context.consume(tokenKind: .forwardSlash, feedback: "Expected a `/`")
			let _ = try? context.consume(tokenKind: .bang, feedback: "Expected a `!`")
			let identifier = try context.consume(tokenKind: .text, feedback: "Expected a tag name")
			
			let attributes = context.untilThrowOrEndOfTokensReached(perform: {
				try context.attempt(action: {
					try Attribute.parse(context: context, options: options)
				})
			})
			
			// If there's a trailing slash (eg <img />), consume it but ignore it. this is invalid html
			_ = try? context.consume(tokenKind: .forwardSlash, feedback: "Expected a trailing `/`")
			return Tag(element: identifier.body, isEnd: slashToken != nil, attributes: attributes)
		}
	}
}

struct Attribute: Hashable, Parsable {
	let key: String
	let value: String
	
	enum AttributeParseError: Error {
		case emptyAttributeValue(key: String)
	}
	
	static func parse(context: ParsingContext, options: ParsingOptions?) throws -> Attribute {
		// todo: attribute keys can be hyphenated
		let key = try context.consume(tokenKind: .text, feedback: "Expected an attribute name")
		
		guard let _ = try? context.consume(tokenKind: .equals, feedback: "Expected an equals sign") else {
			return Attribute(key: key.body, value: key.body)
		}
		
		let value = try context.choose(from: [
			{
				try context.consumeBetween(leftToken: .doubleQuote, rightToken: .doubleQuote) {
					let textContents = context.untilThrowOrEndOfTokensReached {
						try context.consume(where: { $0.kind != .doubleQuote }, skipWhitespaceTokens: false, feedback: "Expected a non `\"` token")
					}
					
					return textContents
						.map(\.body)
						.joined()
				}
			},
			{
				try context.consumeBetween(leftToken: .singleQuote, rightToken: .singleQuote) {
					let textContents = context.untilThrowOrEndOfTokensReached {
						try context.consume(where: { $0.kind != .singleQuote }, skipWhitespaceTokens: false, feedback: "Expected a non `'` token")
					}
					
					return textContents
						.map(\.body)
						.joined()
				}
			},
			{
				let textContents = context.untilThrowOrEndOfTokensReached {
					try context.consume(
						where: {
							$0.kind != .singleQuote && $0.kind != .doubleQuote && $0.kind != .whitespace && $0.kind != .closeAngleBracket
					},
						skipWhitespaceTokens: false,
						feedback: "Expected non-whitespace, non-quote characters")
				}
				
				guard textContents.isEmpty == false else {
					throw AttributeParseError.emptyAttributeValue(key: key.body)
				}
				
				return textContents
					.map(\.body)
					.joined()
			}
		])
		
		return Attribute(key: key.body, value: value)
	}
}

extension ParsingContext {
	@discardableResult
	func consumeBetween<ContentType>(leftToken: Token.Kind, rightToken: Token.Kind, content: () throws -> ContentType) throws -> ContentType {
		try consume(tokenKind: leftToken, feedback: "Expected a \(leftToken)")
		let consumedContent = try content()
		try consume(tokenKind: rightToken, feedback: "Expected a \(rightToken)")
		
		return consumedContent
	}
}

extension Node {
	var attributeDictionary: [String: String] {
		Dictionary(uniqueKeysWithValues: attributes.map({ ($0.key, $0.value) }))
	}
	
	var childNodes: [Node] {
		switch content {
		case .voidNode, .text: return []
		case .childNodes(let nodes): return nodes
		}
	}
	
	var childNodesSortedIntoBlocks: [Node] {
		var nodesToReturn = [Node]()
		var inlineElements = [Node]()
		
		for node in childNodes {
			let display = node.styleFromAttributes?.display ?? node.defaultDisplayStyle
			if display == .inline {
				inlineElements.append(node)
			} else {
				if inlineElements.isEmpty == false {
					// make a fake block element that has all these as children
					let wrapper = Node(element: "p", content: .childNodes(inlineElements), attributes: [])
					// and append it to our list to return
					nodesToReturn.append(wrapper)
					// then, empty the inlineElements list
					inlineElements = []
				}
				nodesToReturn.append(node)
			}
		}
		if inlineElements.isEmpty == false {
			// make a fake block element that has all these as children
			let wrapper = Node(element: "p", content: .childNodes(inlineElements), attributes: [])
			// and append it to our list to return
			nodesToReturn.append(wrapper)
		}
		return nodesToReturn
	}
	
	var defaultDisplayStyle: Style.DisplayStyle {
		isInlineNode ? .inline : .block
	}
	
	var isInlineNode: Bool {
		[InternalElement.textRun, "a", "abbr", "acronym", "audio", "b", "bdi", "bdo", "big", "br", "button", "canvas", "cite", "code", "data", "datalist", "del", "dfn", "em", "embed", "i", "iframe", "img", "input", "ins", "kbd", "label", "map", "mark", "meter", "noscript", "object", "output", "picture", "progress", "q", "ruby", "s", "samp", "script", "select", "slot", "small", "span", "strong", "sub", "sup", "svg", "template", "textarea", "time", "u", "tt", "var", "video", "wbr"].contains(element)
	}
	
	var textContent: String? {
		switch content {
		case .childNodes, .voidNode: return nil
		case .text(let text): return text
		}
	}
	
	func firstDirectChild(named element: String) -> Node? {
		childNodes.first(where: { $0.element == element })
	}
	
	var styleFromAttributes: Style? {
		guard let styleAttribute = attributeDictionary["style"] else { return nil }
		let stylePairs = styleAttribute.components(separatedBy: ";")
		return Style(
			rawPairs: .init(
				uniqueKeysWithValues: stylePairs
					.map { $0.components(separatedBy: ":") }
					.map { ($0.first?.trimmingCharacters(in: .whitespacesAndNewlines), $0.last?.trimmingCharacters(in: .whitespacesAndNewlines)) }
					.compactMap {
						guard let key = $0, let value = $1 else { return nil }
						return (key, value)
					}
			)
		)
	}
}

struct Style {
	enum DisplayStyle { case inline, block }
	
	var display: DisplayStyle? {
		guard let displayString = rawValue["display"] else { return nil }
		switch displayString {
		case "inline": return .inline
		case "block": return .block
		default:
			return nil
		}
	}
	
	var maxWidth: CGFloat? {
		guard let maxWidth = rawValue["max-width"] else { return nil }
		return WebSize(rawValue: maxWidth).dimension
	}
	
	private let rawValue: [String: String]
	
	init(rawPairs: [String: String]) {
		self.rawValue = rawPairs
	}
}
