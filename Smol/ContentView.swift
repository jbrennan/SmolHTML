//
//  ContentView.swift
//  Smol
//
//  Created by Jason Brennan on 2/9/23.
//

import SwiftUI

struct ContentView: View {
    var body: some View {
        VStack {
            Image(systemName: "globe")
                .imageScale(.large)
                .foregroundColor(.accentColor)
            Text("Hello, world!")
        }
        .padding()
    }
}

struct ContentView_Previews: PreviewProvider {
    static var previews: some View {
        ContentView()
    }
}

struct Token: Equatable, CustomDebugStringConvertible {
	
	enum Kind: Equatable {
		case openAngleBracket, closeAngleBracket, forwardSlash, equals, hyphen, doubleQuote, text, whitespace
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
		case "\"": self.init(kind: .doubleQuote, body: "\"")
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
	static func parse(context: ParsingContext) throws -> Self
}

struct Node: Equatable, Parsable {
	
	static let textRunElement = "__textRun"
	
	enum Content: Equatable {
		case text(String)
		case childNodes([Node])
		case voidNode
	}
	
	let element: String
	let content: Content
	let attributes: [String: String]
	
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
		guard startTag.isEnd == false else {
			throw NodeParseError.openingTagWasActuallyClosing(tagName: startTag.element)
		}
		
		print("Parsing <\(startTag.element)>...")
		
		if startTag.isVoidElement {
			return Node(element: startTag.element, content: .voidNode, attributes: startTag.attributeDictionary)
		}
		
		
		print("looking for child nodes...")
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
					
					guard contentRun.isEmpty == false && contentRun.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty == false else {
						throw NodeParseError.didNotFindAnyText
					}
					
					return Node(element: Node.textRunElement, content: .text(contentRun), attributes: [:])
				}
			])
		})
		print("done looking for child nodes, found: \(children.map(\.element))")
		
		let endTag = try Tag.parse(context: context)
		guard endTag.isEnd else {
			throw NodeParseError.closingTagWasActuallyOpening(tagName: endTag.element)
		}
		
		guard startTag.element == endTag.element else {
			throw NodeParseError.closingTagDidNotMatchOpeningTag(opening: startTag.element, closing: endTag.element)
		}
		
		print("Done parsing </\(startTag.element)>...")
		
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
				let identifier = try context.consume(tokenKind: .text, feedback: "Expected a tag name")
				
				print("Looking for attributes for tag: \(identifier.body)")
				let attributes = context.untilThrowOrEndOfTokensReached(perform: {
					try context.attempt(action: {
						try Attribute.parse(context: context)
					})
				})
				print("Found attributes: \(attributes)")
				
				// If there's a trailing slash (eg <img />), consume it but ignore it. this is invalid html
				_ = try? context.consume(tokenKind: .forwardSlash, feedback: "Expected a trailing `/`")
				
				return Tag(element: identifier.body, isEnd: slashToken != nil, attributes: attributes)
			},
			rightToken: .closeAngleBracket
		)
	}
}

struct Attribute: Parsable {
	let key: String
	let value: String
	
//	enum AttributeParseError: Error {
//		case
//	}
	
	static func parse(context: ParsingContext) throws -> Attribute {
		// todo: attribute keys can be hyphenated
		print("Parsing an attribute...")
		let key = try context.consume(tokenKind: .text, feedback: "Expected an attribute name")
		try context.consume(tokenKind: .equals, feedback: "Expected an equals sign")
		// todo: non-quoted values
		let value = try context.consumeBetween(
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
		print("Done parsing attribute. key: \(key), value: \(value)")
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
