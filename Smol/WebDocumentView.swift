//
//  ContentView.swift
//  Smol
//
//  Created by Jason Brennan on 2/9/23.
//

import SwiftUI

struct BrowserView: View {
	@ObservedObject var controller: PageController
	@State var address: String = "https://nearthespeedoflight.com"
	
	var body: some View {
		VStack(spacing: 0) {
			HStack {
				HStack(spacing: 0) {
					Button(action: {
						controller.goBack()
					}) {
						Image(systemName: "arrowtriangle.left.fill")
					}
					Button(action: {
						controller.goForward()
					}) {
						Image(systemName: "arrowtriangle.right.fill")
					}
				}
				TextField("Address", text: $address)
					.onSubmit {
						guard let url = URL(string: address) else {
							return
						}
						controller.loadPage(at: url)
					}
					.textFieldStyle(RoundedBorderTextFieldStyle())
			}
			.padding()
			Divider()
			WebDocumentView(controller: controller)
				.background(.white)
				.environment(\.openURL, .init(handler: { url in
					controller.loadPage(at: url)
					return .handled
				}))
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
						.firstDirectChild(named: Node.textRunElement)?
						.textContent ?? "Smol"
				)
				.environment(\.font, Font.custom("Times", size: 16))
		}
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
	
	@Published var state = State.notLoaded
	private var previousDocuments: [(Document, URL)] = []
	
	private var currentlyLoadedURL: URL? {
		switch state {
		case .notLoaded, .failed: return nil
		case .loaded(_, let url): return url
		}
	}
	
	func loadPage(at url: URL) {
		Task {
			let fullURL: URL
			if url.host != nil {
				fullURL = url
			} else {
				guard let previousURL = currentlyLoadedURL else {
					print("Could not load url \(url) because it didn't have a host and we don't have a previously loaded host either.")
					self.state = .failed(LoadingError.failedToLoad(url))
					return
				}
				fullURL = URL(string: url.path, relativeTo: previousURL)!
			}
			let (data, response) = try await URLSession.shared.data(from: fullURL)
			let htmlString = String(data: data, encoding: .utf8) ?? ""
			let tokenizer = Tokenizer(programText: htmlString)
			let context = try ParsingContext(tokens: tokenizer.scanAllTokens())
			
			await MainActor.run {
				do {
					let oldState = state
					state = .loaded(try Document.parse(context: context), response.url ?? fullURL)
					switch oldState {
					case .failed, .notLoaded: break
					case .loaded(let oldDocument, let oldURL):
						previousDocuments.append((oldDocument, oldURL))
					}
				} catch {
					print("error parsing document: \(error)")
					state = .failed(error)
				}
			}
		}
	}
	
	func goBack() {
		guard let (previousDocument, previousURL) = previousDocuments.popLast() else { return }
		state = .loaded(previousDocument, previousURL)
	}
	func goForward() {}
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
	let defaultFont: Font
	
	var body: some View {
		Text(
			node
				.childNodes
				.map { $0.attributedText(defaultFont: defaultFont) }
				.reduce(AttributedString(), +)
		)
	}
}


struct ImageView: View {
	let node: Node
	var body: some View {
		Image(node.attributes["src"] ?? "")
			.resizable()
			.aspectRatio(contentMode: .fill)
			.frame(width: node.attributes["width"].flatMap(WebSize.init(rawValue:))?.dimension)
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
	
	var body: some View {
		VStack(alignment: .leading, spacing: 20) {
			ForEach(children, id: \.self) { childNode in
				switch childNode.element {
				case "h1":
					InlineContentWrappingBlockView(node: childNode, defaultFont: Font.custom("Times", size: 32).bold())
				case "h2":
					InlineContentWrappingBlockView(node: childNode, defaultFont: Font.custom("Times", size: 28).bold())
				case "p":
					InlineContentWrappingBlockView(node: childNode, defaultFont: Font.custom("Times", size: 16))
				case "img": ImageView(node: childNode)
				case "div", "section", "footer", "article", "header", "nav":
					BlocksView(children: childNode.childNodesSortedIntoBlocks)
				case "blockquote":
					BlocksView(children: childNode.childNodesSortedIntoBlocks)
						.padding(.leading, 20)
				default: Text("unknown block element: <\(childNode.element)>")
				}
			}
		}
	}
}

struct BodyView: View {
	let bodyNode: Node
	var body: some View {
		ScrollView {
			BlocksView(children: bodyNode.childNodesSortedIntoBlocks)
			.padding(20)
		}
		.frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .topLeading)
		.background(Color.white)
	}
}

extension Node {
	func attributedText(defaultFont: Font) -> AttributedString {
		switch element {
		case Node.textRunElement:
			var attributes = AttributeContainer()
			attributes.font = defaultFont
			
			return AttributedString(textContent ?? "", attributes: attributes)
		case "em":
			var attributes = AttributeContainer()
			attributes.font = defaultFont.italic()
			
			return childNodes
				.map { $0.attributedText(defaultFont: defaultFont) }
				.reduce(AttributedString(), +)
				.mergingAttributes(attributes, mergePolicy: .keepCurrent)
		case "strong":
			var attributes = AttributeContainer()
			attributes.font = defaultFont.bold()
			
			return childNodes
				.map { $0.attributedText(defaultFont: defaultFont) }
				.reduce(AttributedString(), +)
				.mergingAttributes(attributes, mergePolicy: .keepCurrent)
		case "a":
			var attributes = AttributeContainer()
			attributes.link = URL(string: self.attributes["href"] ?? "")
			attributes.underlineStyle = .single
			
			return childNodes
				.map { $0.attributedText(defaultFont: defaultFont) }
				.reduce(AttributedString(), +)
				.mergingAttributes(attributes, mergePolicy: .keepCurrent)
		default: return AttributedString()
		}
	}
}

struct Token: Equatable, CustomDebugStringConvertible {
	
	enum Kind: Equatable {
		case openAngleBracket, closeAngleBracket, forwardSlash, equals, hyphen, singleQuote, doubleQuote, text, whitespace, bang
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
	
	// todo: unsure about the logic here...
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
	let cursor: ScanningCursor
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
			// todo: group whitespace?
			// todo: differentiate between newlines and other whitespace?
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
	
	private enum TokenizerError: Error {
		case unknownToken(Character)
	}
}

private extension Character {
	var isIdentifierCharacter: Bool {
		isLetter || isWholeNumber
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
		var results = [ConsumedType]()
		
		do {
			while isNotAtEnd {
				results.append(try perform())
			}
		} catch {
			return results
		}
		return results
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
	
	func advance(when predicate: (Token) -> Bool, skipWhitespaceTokens: Bool = true) -> Bool {
		
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
	static func parse(context: ParsingContext) throws -> Self
}

/// This type mostly exists right now to handle parsing pages that have a `<!doctype>` node at their root, along with an `<html>` node.
/// For now, we're discarding the doctype.
struct Document: Hashable, Parsable {
	
	enum DocumentError: Error {
		case unableToFindHTMLNode
	}
	
	let htmlNode: Node
	
	static func parse(context: ParsingContext) throws -> Document {
		let (nodes, error) = context.untilErrorThrownOrEndOfTokensReached {
			try Node.parse(context: context)
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
	
	static let textRunElement = "__textRun"
	private static let commentElement = "__comment"
	
	enum Content: Hashable {
		case text(String)
		case childNodes([Node])
		case voidNode
	}
	
	let element: String
	let content: Content
	let attributes: [String: String]
	let id = UUID()
	
	enum NodeParseError: Error {
		case closingTagDidNotMatchOpeningTag(opening: String, closing: String)
		
		/// This error will probably get thrown a lot, just to signify that parsing a child failed.
		/// todo: It's probably wasteful to do it this way!
		case openingTagWasActuallyClosing(tagName: String)
		
		case closingTagWasActuallyOpening(tagName: String)
		case didNotFindAnyText
	}
	
	static func parse(context: ParsingContext) throws -> Node {
		
		let startTag = try Tag.parse(context: context)
//		print("got start tag: \(startTag)")
		guard startTag.isEnd == false else {
			throw NodeParseError.openingTagWasActuallyClosing(tagName: startTag.element)
		}
		
//		print("Parsing <\(startTag.element)>...")
		
		if startTag.element.lowercased() == "doctype" {
//			print("Done parsing \(startTag.element)\n")
			return Node(element: "doctype", content: .voidNode, attributes: startTag.attributeDictionary)
		}
		
		if startTag.isVoidElement {
//			print("Done parsing \(startTag.element)\n")
			return Node(element: startTag.element, content: .voidNode, attributes: startTag.attributeDictionary)
		}
		
		
//		print("looking for child nodes...")
		let children = context.untilThrowOrEndOfTokensReached(perform: {
			try context.choose(from: [
				{ try Node.parse(context: context) },
				{
					let textContents = context.untilThrowOrEndOfTokensReached {
						try context.consume(where: { $0.kind != .openAngleBracket }, skipWhitespaceTokens: false, feedback: "Expected a non `<` token")
					}
					guard textContents.isEmpty == false else {
						throw NodeParseError.didNotFindAnyText
					}
					
					// todo: this does not follow the exact html rules, but good enough for now
					let contentRun = textContents
						.map(\.body)
						.joined()
						.replacingOccurrences(of: "\n", with: " ")
						.replacingOccurrences(of: "\t", with: " ")
						.replacingOccurrences(of: "&#x000A;", with: "")
					
//					guard contentRun.isEmpty == false && contentRun.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty == false else {
//						throw NodeParseError.didNotFindAnyText
//					}
					
					return Node(element: Node.textRunElement, content: .text(contentRun), attributes: [:])
				},
				{
					try context.consumeBetween(
						leftToken: .openAngleBracket,
						content: {
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
							
							return Node(element: Node.commentElement, content: .voidNode, attributes: [:])
						},
						rightToken: .closeAngleBracket
					)
				}
			])
		})
			.filter {
				if $0.element == Node.commentElement { return false }
				if $0.element != Node.textRunElement { return true }
				
				// filter out empty text run nodes
				return $0.textContent?.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty == false
			}
		
//		print("done looking for child nodes, found: \(children.map(\.element))")
		
		let endTag = try Tag.parse(context: context)
		guard endTag.isEnd else {
			throw NodeParseError.closingTagWasActuallyOpening(tagName: endTag.element)
		}
		
		guard startTag.element == endTag.element else {
			throw NodeParseError.closingTagDidNotMatchOpeningTag(opening: startTag.element, closing: endTag.element)
		}
		
//		print("Done parsing </\(startTag.element)>...\n")
		
		return .init(
			element: startTag.element,
			content: .childNodes(children),
			attributes: startTag.attributeDictionary
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
	
	var attributeDictionary: [String: String] {
		Dictionary(uniqueKeysWithValues: attributes.map({ ($0.key, $0.value) }))
	}
	
	static func parse(context: ParsingContext) throws -> Tag {
		try context.consumeBetween(
			leftToken: .openAngleBracket,
			content: {
				let slashToken = try? context.consume(tokenKind: .forwardSlash, feedback: "Expected a `/`")
				let _ = try? context.consume(tokenKind: .bang, feedback: "Expected a `!`")
				let identifier = try context.consume(tokenKind: .text, feedback: "Expected a tag name")
				
//				print("Looking for attributes for tag: \(identifier.body)")
				let attributes = context.untilThrowOrEndOfTokensReached(perform: {
					try context.attempt(action: {
						try Attribute.parse(context: context)
					})
				})
//				print("Found attributes: \(attributes)")
				
				// If there's a trailing slash (eg <img />), consume it but ignore it. this is invalid html
				_ = try? context.consume(tokenKind: .forwardSlash, feedback: "Expected a trailing `/`")
//				print("about to finish parsing tag: \(identifier.body)")
				return Tag(element: identifier.body, isEnd: slashToken != nil, attributes: attributes)
			},
			rightToken: .closeAngleBracket
		)
	}
}

struct Attribute: Parsable {
	let key: String
	let value: String
	
	enum AttributeParseError: Error {
		case emptyAttributeValue(key: String)
	}
	
	static func parse(context: ParsingContext) throws -> Attribute {
		// todo: attribute keys can be hyphenated
//		print("Parsing an attribute...")
		let key = try context.consume(tokenKind: .text, feedback: "Expected an attribute name")
		
		
		
		guard let _ = try? context.consume(tokenKind: .equals, feedback: "Expected an equals sign") else {
//			print("Done parsing key-only attribute: \(key.body)")
			return Attribute(key: key.body, value: key.body)
		}
		
		let value = try context.choose(from: [
			{
				try context.consumeBetween(
					leftToken: .doubleQuote,
					content: {
						let textContents = context.untilThrowOrEndOfTokensReached {
							try context.consume(where: { $0.kind != .doubleQuote }, skipWhitespaceTokens: false, feedback: "Expected a non `\"` token")
						}
						
						return textContents
							.map(\.body)
							.joined()
					},
					rightToken: .doubleQuote
				)
			},
			{
				try context.consumeBetween(
					leftToken: .singleQuote,
					content: {
						let textContents = context.untilThrowOrEndOfTokensReached {
							try context.consume(where: { $0.kind != .singleQuote }, skipWhitespaceTokens: false, feedback: "Expected a non `'` token")
						}
						
						return textContents
							.map(\.body)
							.joined()
					},
					rightToken: .singleQuote
				)
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
		
//		print("Done parsing attribute. key: \(key), value: \(value)")
		return Attribute(key: key.body, value: value)
	}
}

extension ParsingContext {
	@discardableResult
	func consumeBetween<ContentType>(leftToken: Token.Kind, content: () throws -> ContentType, rightToken: Token.Kind) throws -> ContentType {
		try consume(tokenKind: leftToken, feedback: "Expected a \(leftToken)")
		let consumedContent = try content()
		try consume(tokenKind: rightToken, feedback: "Expected a \(rightToken)")
		
		return consumedContent
	}
}

extension Node {
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
			if node.isInlineNode {
				inlineElements.append(node)
			} else {
				if inlineElements.isEmpty == false {
					// make a fake block element that has all these as children
					let wrapper = Node(element: "p", content: .childNodes(inlineElements), attributes: [:])
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
			let wrapper = Node(element: "p", content: .childNodes(inlineElements), attributes: [:])
			// and append it to our list to return
			nodesToReturn.append(wrapper)
		}
		return nodesToReturn
	}
	
	var isInlineNode: Bool {
		[Node.textRunElement, "a", "abbr", "acronym", "audio", "b", "bdi", "bdo", "big", "br", "button", "canvas", "cite", "code", "data", "datalist", "del", "dfn", "em", "embed", "i", "iframe", "img", "input", "ins", "kbd", "label", "map", "mark", "meter", "noscript", "object", "output", "picture", "progress", "q", "ruby", "s", "samp", "script", "select", "slot", "small", "span", "strong", "sub", "sup", "svg", "template", "textarea", "time", "u", "tt", "var", "video", "wbr"].contains(element)
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
}