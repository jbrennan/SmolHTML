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
			Token(kind: .whitespace, body: " "),
			Token(kind: .openAngleBracket, body: "<"),
			Token(kind: .forwardSlash, body: "/"),
			Token(kind: .text, body: "html"),
			Token(kind: .closeAngleBracket, body: ">"),
		])
	}
	
	func testTokenizerSplitsOnWhitespace() throws {
		let program1 = "img src"
		let tokenizer = Tokenizer(programText: program1)
		
		let tokens = try tokenizer.scanAllTokens()
		
		XCTAssertEqual(tokens, [
			Token(kind: .text, body: "img"),
			Token(kind: .whitespace, body: " "),
			Token(kind: .text, body: "src"),
		])
	}
	
	
	// MARK: - Node parsing

	func testEmptyHTMLTag() throws {
		let program1 = "<html></html>"
		let tokenizer = Tokenizer(programText: program1)
		
		let tokens = try tokenizer.scanAllTokens()
		
		let context = ParsingContext(tokens: tokens)
		let node = try Node.parse(context: context)
		
		XCTAssertEqual(node, Node(element: "html", content: .childNodes([]), attributes: [:]))
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
		
		XCTAssertEqual(node, Node(element: "html", content: .childNodes([Node(element: "body", content: .childNodes([]), attributes: [:])]), attributes: [:]))
	}
	
	func testDoubleNestedTag() throws {
		let program1 = "<html>\n<body>\n<h1></h1>\n</body>\n</html>"
		let tokenizer = Tokenizer(programText: program1)
		
		let tokens = try tokenizer.scanAllTokens()
		
		let context = ParsingContext(tokens: tokens)
		let node = try Node.parse(context: context)
		
		XCTAssertEqual(node, Node(element: "html", content: .childNodes([Node(element: "body", content: .childNodes([Node(element: "h1", content: .childNodes([]), attributes: [:])]), attributes: [:])]), attributes: [:]))
	}
	
	func testTagWithJustText() throws {
		let program1 = "<p>hello there</p>"
		let tokenizer = Tokenizer(programText: program1)
		
		let tokens = try tokenizer.scanAllTokens()
		
		let context = ParsingContext(tokens: tokens)
		let node = try Node.parse(context: context)
		
		XCTAssertEqual(node, Node(element: "p", content: .childNodes([Node(element: Node.textRunElement, content: .text("hello there"), attributes: [:])]), attributes: [:]))
	}
	
	func testTagWithTextAndChildTags() throws {
		let program1 = "<p>hello <em>there</em></p>"
		let tokenizer = Tokenizer(programText: program1)
		
		let tokens = try tokenizer.scanAllTokens()
		
		let context = ParsingContext(tokens: tokens)
		let node = try Node.parse(context: context)
		
		XCTAssertEqual(node, Node(element: "p", content: .childNodes([Node(element: Node.textRunElement, content: .text("hello "), attributes: [:]), Node(element: "em", content: .childNodes([Node(element: Node.textRunElement, content: .text("there"), attributes: [:])]), attributes: [:])]), attributes: [:]))
	}
	
	func testVoidElement() throws {
		let program1 = "<img>"
		let tokenizer = Tokenizer(programText: program1)
		
		let tokens = try tokenizer.scanAllTokens()
		
		let context = ParsingContext(tokens: tokens)
		let node = try Node.parse(context: context)
		
		XCTAssertEqual(node, Node(element: "img", content: .voidNode, attributes: [:]))
	}
	
	func testVoidElementWithAttributes() throws {
		let program1 = "<img src=\"http://example.com/image.png\" width=\"600px\">"
		let tokenizer = Tokenizer(programText: program1)
		
		let tokens = try tokenizer.scanAllTokens()
		
		let context = ParsingContext(tokens: tokens)
		let node = try Node.parse(context: context)
		
		XCTAssertEqual(node, Node(element: "img", content: .voidNode, attributes: ["src": "http://example.com/image.png", "width": "600px"]))
	}
	
	func testVoidElementWithKeyOnlyAttributes() throws {
		let program1 = "<img src autoplay me=\"you\" blep>"
		let tokenizer = Tokenizer(programText: program1)
		
		let tokens = try tokenizer.scanAllTokens()
		
		let context = ParsingContext(tokens: tokens)
		let node = try Node.parse(context: context)
		
		XCTAssertEqual(node, Node(element: "img", content: .voidNode, attributes: ["src": "src", "autoplay": "autoplay", "me": "you", "blep": "blep"]))
	}
	
//	func testVoidElementWithHyphenatedAttributeKey() throws {
//		let program1 = "<img data-name=\"hello\">"
//		let tokenizer = Tokenizer(programText: program1)
//
//		let tokens = try tokenizer.scanAllTokens()
//
//		let context = ParsingContext(tokens: tokens)
//		let node = try Node.parse(context: context)
//
//		XCTAssertEqual(node, Node(element: "img", content: .voidNode, attributes: ["data-name": "hello"]))
//	}
	
	func testVoidElementWithTrailingSlash() throws {
		let program1 = "<img/>"
		let tokenizer = Tokenizer(programText: program1)
		
		let tokens = try tokenizer.scanAllTokens()
		
		let context = ParsingContext(tokens: tokens)
		let node = try Node.parse(context: context)
		
		XCTAssertEqual(node, Node(element: "img", content: .voidNode, attributes: [:]))
	}
	
	func testVoidElementWithSpaceAndTrailingSlash() throws {
		let program1 = "<img />"
		let tokenizer = Tokenizer(programText: program1)
		
		let tokens = try tokenizer.scanAllTokens()
		
		let context = ParsingContext(tokens: tokens)
		let node = try Node.parse(context: context)
		
		XCTAssertEqual(node, Node(element: "img", content: .voidNode, attributes: [:]))
	}
	
	func testNestedVoidElement() throws {
		let program1 = "<html><body><img></body></html>"
		let tokenizer = Tokenizer(programText: program1)
		
		let tokens = try tokenizer.scanAllTokens()
		
		let context = ParsingContext(tokens: tokens)
		let node = try Node.parse(context: context)
		
		XCTAssertEqual(node, Node(element: "html", content: .childNodes([Node(element: "body", content: .childNodes([Node(element: "img", content: .voidNode, attributes: [:])]), attributes: [:])]), attributes: [:]))
	}
	
	func testSkipsCommentTags() throws {
		let program1 = "<html><!-- comment --><head>hi</head></html>"
		let tokenizer = Tokenizer(programText: program1)
		
		let tokens = try tokenizer.scanAllTokens()
		
		let context = ParsingContext(tokens: tokens)
		let node = try Node.parse(context: context)
		
		XCTAssertEqual(node, Node(element: "html", content: .childNodes([Node(element: "head", content: .childNodes([Node(element: Node.textRunElement, content: .text("hi"), attributes: [:])]), attributes: [:])]), attributes: [:]))
	}

}
