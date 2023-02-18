//
//  SmolTests.swift
//  SmolTests
//
//  Created by Jason Brennan on 2/9/23.
//

import XCTest
@testable import Smol

final class SmolTests: XCTestCase {
	
	// MARK: - Tokenizer
	
	func testTokenizer() throws {
		let program1 = "<html> </html>"
		let tokenizer = Tokenizer(programText: program1)
		
		let tokens = try tokenizer.scanAllTokens()
		
		XCTAssertEqual(tokens, [
			Token(kind: .openAngleBracket, body: "<"),
			Token(kind: .text, body: "html"),
			Token(kind: .closeAngleBracket, body: ">"),
			Token(kind: .text, body: " "),
			Token(kind: .openAngleBracket, body: "<"),
			Token(kind: .forwardSlash, body: "/"),
			Token(kind: .text, body: "html"),
			Token(kind: .closeAngleBracket, body: ">"),
		])
	}
	
	
	// MARK: - Node parsing

	func testEmptyHTMLTag() throws {
		let program1 = "<html></html>"
		let tokenizer = Tokenizer(programText: program1)
		
		let tokens = try tokenizer.scanAllTokens()
		
		let context = ParsingContext(tokens: tokens)
		let node = try Node.parse(context: context)
		
		XCTAssertEqual(node, Node(element: "html", content: .childNodes([])))
    }
	
	func testIncorrectlyMatchingTagFails() throws {
		let program1 = "<html></body>"
		let tokenizer = Tokenizer(programText: program1)
		
		let tokens = try tokenizer.scanAllTokens()
		
		let context = ParsingContext(tokens: tokens)
		XCTAssertThrowsError(try Node.parse(context: context))
	}
	
	func testHTMLTagWithBody() throws {
		let program1 = "<html><body></body></html>"
		let tokenizer = Tokenizer(programText: program1)
		
		let tokens = try tokenizer.scanAllTokens()
		
		let context = ParsingContext(tokens: tokens)
		let node = try Node.parse(context: context)
		
		XCTAssertEqual(node, Node(element: "html", content: .childNodes([Node(element: "body", content: .childNodes([]))])))
	}
	
	func testDoubleNestedTag() throws {
		let program1 = "<html><body><h1></h1></body></html>"
		let tokenizer = Tokenizer(programText: program1)
		
		let tokens = try tokenizer.scanAllTokens()
		
		let context = ParsingContext(tokens: tokens)
		let node = try Node.parse(context: context)
		
		XCTAssertEqual(node, Node(element: "html", content: .childNodes([Node(element: "body", content: .childNodes([Node(element: "h1", content: .childNodes([]))]))])))
	}
	
	func testTagWithJustText() throws {
		let program1 = "<p>hello</p>"
		let tokenizer = Tokenizer(programText: program1)
		
		let tokens = try tokenizer.scanAllTokens()
		
		let context = ParsingContext(tokens: tokens)
		let node = try Node.parse(context: context)
		
		XCTAssertEqual(node, Node(element: "p", content: .childNodes([Node(element: Node.textRunElement, content: .text("hello"))])))
	}
	
	func testTagWithTextAndChildTags() throws {
		let program1 = "<p>hello <em>there</em></p>"
		let tokenizer = Tokenizer(programText: program1)
		
		let tokens = try tokenizer.scanAllTokens()
		
		let context = ParsingContext(tokens: tokens)
		let node = try Node.parse(context: context)
		
		XCTAssertEqual(node, Node(element: "p", content: .childNodes([Node(element: Node.textRunElement, content: .text("hello ")), Node(element: "em", content: .childNodes([Node(element: Node.textRunElement, content: .text("there"))]))])))
	}
	
	func testVoidElement() throws {
		let program1 = "<img>"
		let tokenizer = Tokenizer(programText: program1)
		
		let tokens = try tokenizer.scanAllTokens()
		
		let context = ParsingContext(tokens: tokens)
		let node = try Node.parse(context: context)
		
		XCTAssertEqual(node, Node(element: "img", content: .voidNode))
	}
	
	func testVoidElementWithTrailingSlash() throws {
		let program1 = "<img/>"
		let tokenizer = Tokenizer(programText: program1)
		
		let tokens = try tokenizer.scanAllTokens()
		
		let context = ParsingContext(tokens: tokens)
		let node = try Node.parse(context: context)
		
		XCTAssertEqual(node, Node(element: "img", content: .voidNode))
	}
	
//	func testVoidElementWithSpaceAndTrailingSlash() throws {
//		// this test currently fails because it thinks the element is called "img " (trailing space).
//		// that tag doesn't match "img" or any other void element, so the parser thinks it's a normal element, and looks for an end tag
//		// which it can't find.
//		// I'll probably fix this by making the tokenizer a little more fine-grained in how it accepts whitespace.
//		let program1 = "<img />"
//		let tokenizer = Tokenizer(programText: program1)
//		
//		let tokens = try tokenizer.scanAllTokens()
//		
//		let context = ParsingContext(tokens: tokens)
//		let node = try Node.parse(context: context)
//		
//		XCTAssertEqual(node, Node(element: "img", content: .voidNode))
//	}
	
	func testNestedVoidElement() throws {
		let program1 = "<html><body><img></body></html>"
		let tokenizer = Tokenizer(programText: program1)
		
		let tokens = try tokenizer.scanAllTokens()
		
		let context = ParsingContext(tokens: tokens)
		let node = try Node.parse(context: context)
		
		XCTAssertEqual(node, Node(element: "html", content: .childNodes([Node(element: "body", content: .childNodes([Node(element: "img", content: .voidNode)]))])))
	}

}
