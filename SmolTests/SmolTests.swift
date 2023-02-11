//
//  SmolTests.swift
//  SmolTests
//
//  Created by Jason Brennan on 2/9/23.
//

import XCTest
@testable import Smol

final class SmolTests: XCTestCase {

    func testEmptyHTMLTag() throws {
		let program1 = "<html> </html>"
		let tokenizer = Tokenizer(programText: program1)
		
		let tokens = try tokenizer.scanAllTokens()
		
		let context = ParsingContext(tokens: tokens)
		let node = try Node.parse(context: context)
		
		XCTAssertEqual(node, Node(element: "html", childNodes: []))
    }
	
	func testIncorrectlyMatchingTagFails() throws {
		let program1 = "<html> </body>"
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
		
		XCTAssertEqual(node, Node(element: "html", childNodes: [Node(element: "body", childNodes: [])]))
	}
	
	func testDoubleNestedTag() throws {
		let program1 = "<html><body><h1></h1></body></html>"
		let tokenizer = Tokenizer(programText: program1)
		
		let tokens = try tokenizer.scanAllTokens()
		
		let context = ParsingContext(tokens: tokens)
		let node = try Node.parse(context: context)
		
		XCTAssertEqual(node, Node(element: "html", childNodes: [Node(element: "body", childNodes: [Node(element: "h1", childNodes: [])])]))
	}

}
