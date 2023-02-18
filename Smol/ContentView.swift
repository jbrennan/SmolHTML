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
		case openAngleBracket, closeAngleBracket, forwardSlash, text
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
	
	var currentToken: Token { tokens[currentTokenIndex] }
	
	init(tokens: [Token]) {
		self.tokens = tokens
	}
	
	enum ParseError: Error {
		case unexpectedToken(Token, feedback: String)
		case failedToParse
	}
	
	@discardableResult
	func consume(tokenKind kind: Token.Kind, feedback: String) throws -> Token {
		let currentToken = self.currentToken
		guard advance(when: { $0.kind == kind }) else {
			throw ParseError.unexpectedToken(currentToken, feedback: feedback)
		}
		return currentToken
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
	
	private func advance(when predicate: (Token) -> Bool) -> Bool {
		guard isNotAtEnd else { return false }
		
		if predicate(tokens[currentTokenIndex]) {
			currentTokenIndex += 1
			return true
		} else {
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
	
	enum NodeParseError: Error {
		case closingTagDidNotMatchOpeningTag(opening: String, closing: String)
		
		/// This error will probably get thrown a lot, just to signify that parsing a child failed.
		/// todo: It's probably wasteful to do it this way!
		case openingTagWasActuallyClosing(tagName: String)
		
		case closingTagWasActuallyOpening(tagName: String)
		case didNotFindAnyText
	}
	
	static func parse(context: ParsingContext) throws -> Node {
		// "<", identifier, ">", 0-or-more children, "<", "/", identifier, ">"
		let openingTag = try Tag.parse(context: context)
		guard openingTag.isClosing == false else {
			throw NodeParseError.openingTagWasActuallyClosing(tagName: openingTag.element)
		}
		
		print("Parsing <\(openingTag.element)>...")
		
		// todo: we're currently ignoring "void elements"
		
		print("looking for child nodes...")
		let children = context.untilThrowOrEndOfTokensReached(perform: {
			try context.choose(from: [
				{ try Node.parse(context: context) },
				{
					let textContents = context.untilThrowOrEndOfTokensReached {
						try context.consume(tokenKind: .text, feedback: "Expected text contents")
					}
					guard textContents.isEmpty == false else {
						throw NodeParseError.didNotFindAnyText
					}
					
					return Node(element: Node.textRunElement, content: .text(textContents.map(\.body).joined()))
				}
			])
		})
		print("done looking for child nodes, found: \(children.map(\.element))")
		
		let closingTag = try Tag.parse(context: context)
		guard closingTag.isClosing else {
			throw NodeParseError.closingTagWasActuallyOpening(tagName: closingTag.element)
		}
		
		guard openingTag.element == closingTag.element else {
			throw NodeParseError.closingTagDidNotMatchOpeningTag(opening: openingTag.element, closing: closingTag.element)
		}
		
		print("Done parsing </\(openingTag.element)>...")
		
		return .init(element: openingTag.element, content: .childNodes(children))
	}
}

struct Tag: Parsable {
	let element: String
	let isClosing: Bool
	
	static func parse(context: ParsingContext) throws -> Tag {
		try context.consumeBetween(
			leftToken: .openAngleBracket,
			content: {
				let slashToken = try? context.consume(tokenKind: .forwardSlash, feedback: "Expected a `/`")
				let identifier = try context.consume(tokenKind: .text, feedback: "Expected a tag name")
				
				return Tag(element: identifier.body, isClosing: slashToken != nil)
			},
			rightToken: .closeAngleBracket
		)
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

