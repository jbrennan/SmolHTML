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
		.onAppear {
			// todo: tests
			let program1 = "<html> </html>"
			let tokenizer = Tokenizer(programText: program1)
			do {
				let tokens = try tokenizer.scanAllTokens()
				
				let context = ParsingContext(tokens: tokens)
				let node = try Node.parse(context: context)
				print(node)
			} catch {
				print(error)
			}
		}
    }
}

struct ContentView_Previews: PreviewProvider {
    static var previews: some View {
        ContentView()
    }
}

struct Token {
	
	enum Kind: Equatable {
		case openAngleBracket, closeAngleBracket, forwardSlash, identifier
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
			startNewToken()
			try scanNextToken()
		}
		print(scannedTokens)
		return scannedTokens
	}
	
	private func startNewToken() {}
	private func scanNextToken() throws {
		let next = cursor.advance()
		
		if let token = Token(symbol: next) {
			return scannedTokens.append(token)
		}
		
		if next.isWhitespace {
			return
		} else if next.isLetter {
			return scanIdentifier()
		}
		
		throw TokenizerError.unknownToken(next)
	}
	
	private enum TokenizerError: Error {
		case unknownToken(Character)
	}
	
	private func scanIdentifier() {
		var identifier = String(cursor.previousCharacter())
		while cursor.isNotAtEnd {
			let next = cursor.currentCharacter()
			if next.isLetter {
				identifier.append(next)
				cursor.advance()
			} else {
				break
			}
		}
		
		scannedTokens.append(Token(kind: .identifier, body: identifier))
	}
}

class ParsingContext {
	private let tokens: [Token]
	private var currentTokenIndex = 0
	
	var isNotAtEnd: Bool {
		currentTokenIndex < tokens.count
	}
	
	var currentToken: Token { tokens[currentTokenIndex] }
	
	init(tokens: [Token]) {
		self.tokens = tokens
	}
	
	enum ParseError: Error {
		case unexpectedToken(Token, feedback: String)
	}
	
	@discardableResult
	func consume(tokenKind kind: Token.Kind, feedback: String) throws -> Token {
		let currentToken = self.currentToken
		guard advance(when: { $0.kind == kind }) else {
			throw ParseError.unexpectedToken(currentToken, feedback: feedback)
		}
		return currentToken
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

struct Node: Parsable {
	let element: String
//	let childNodes: [Node]
	
	enum NodeParseError: Error {
		case closingTagDidNotMatchOpeningTag(opening: String, closing: String)
	}
	
	static func parse(context: ParsingContext) throws -> Node {
		// "<", identifier, ">", 0-or-more children, "<", "/", identifier, ">"
		try context.consume(tokenKind: .openAngleBracket, feedback: "Expected a `<`")
		let identifier = try context.consume(tokenKind: .identifier, feedback: "Expected a tag name")
		try context.consume(tokenKind: .closeAngleBracket, feedback: "Expected a `>`")
		
		// todo: we're currently ignoring "void elements"
		// todo: child contents
		
		try context.consume(tokenKind: .openAngleBracket, feedback: "Expected a `<`")
		try context.consume(tokenKind: .forwardSlash, feedback: "Expected a `/`")
		let closingIdentifier = try context.consume(tokenKind: .identifier, feedback: "Expected a tag name")
		guard identifier.body == closingIdentifier.body else {
			throw NodeParseError.closingTagDidNotMatchOpeningTag(opening: identifier.body, closing: closingIdentifier.body)
		}
		try context.consume(tokenKind: .closeAngleBracket, feedback: "Expected a `>`")
		
		return .init(element: identifier.body)
	}
}

