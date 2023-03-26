#  Let's Write a Web Browser from Scratch in Swift!

There's a rumour that Apple's going to start allowing custom, non-WebKit based browser engines on iOS starting later this year. While that most likely means Chrome, Firefox, and the other big browsers could start using custom engines, it also means you could write your own too. So why not try it?

In this 2 part series, I'll take you through how to write a basic web browser, from parsing HTML in Swift to rendering the pages with SwiftUI, displaying them with a simple, but familiar interface. 

You might be thinking "Aren't web browsers huge, incredibly complicated pieces of software?" and yes, the big ones we use every day are huge and complicated. But even huge and complicated pieces of software are still "just software" at their core, written by normal programmers just doing their job or following their passion.

What we're attempting in this series is a very simple browser, and the end result is actually a little under 1000 lines of fairly straightforward Swift code. We'll focus solely on rendering a subset of HTML, leaving CSS and Javascript as exercises for the reader :). But in the end you should have an app that can render unstyled, standard HTML pages. Let's get started!

## The Architecture

Before we dive in to code, let's look at the overall archicture of what we're building, to make the challenge ahead more managable. Since HTML is a programming language, we'll follow a similar architecture to that of most compilers / interpreters, which is a sort of pipeline. That's a fancy way of saying some stuff goes in at one end, gets modified by a component, and gets spit out as some new stuff on the other end, where the next component takes *that* stuff and modifies and outputs it, repeating until we have our page rendered in the UI. It's kind of like a glorified chain of function calls. So what are our inputs and components?

1. We start with the **raw html**, as a `String`. This either comes from the network or a local file, but it doesn't really matter.
2. We then digest the html string into an array of **tokens**, in a process known as *tokenizing* or *lexing*. This essentially chews up the raw string into common pieces that are easier to digest, such as punctuation characters (`<, >, ", etc`), whitespaces (newlines, tabs, spaces), digits, or just regular letter characters.
3. The tokens are then passed to the **Parsing Context**, a class whose core purpose is letting other types *consume* tokens they recognize, while also keeping track of which tokens have already been consumed.
4. Next we have structs representing **the data we're parsing**, things like, the `Document`, a tree of `Node`s, which consist of `Tag`s and `Attribute`s. We'll write little parsers for each of these things, that will call into the `ParsingContext` to consume the tokens they need for their construction.
5. Finally, when parsing is complete, we have data we can then use to **display our SwiftUI pages** with. In a traditional programming language, this might be the point where you output compiled code into an executable or evaluate your data with an interpreter, but here our "interpreter" will simply display a UI.

With that general archictecture in mind, let's fire up Xcode and get started.

### A Quick Tip

If you're coding along as you read this tutorial, I highly recommend typing out all the code yourself, instead of copying and pasting. In my experience, I find this forces you to slow down and work more deliberately, and I think it'll help you understand things better in the process.

I'd also recommend changing things as you move along. None of what I've written is the definitive way to write this code, and you could probably put your own spin on it. Or extend it to do even more!

## Starting the project

Create a new Xcode project using a SwiftUI template. I called my browser `Smol` because it's very tiny, but feel free to let your creativity shine here. I made my browser be a Mac app just for ease of playing around with, but you could make yours an iOS app if you wanted, everything will work more or less the same. I would make sure testing is enabled (but feel free to delete the UI tests target, as we'll really only be using Unit Tests here) 

In your project settings Info tab, add key for "App Transport Security Settings," and inside that add a key for "Allow arbitrary loads," setting its value to Yes. This will let us load http and https content from anywhere on the internet and is not enabled by default.

## Tokenizing

Tokenizing is the process of breaking down our program from a `String` into an array of `Token` elements, by scanning through the program character by character as we find different tokens. We'll make 3 types: `Token`, `ScanningCursor`, and `Tokenizer`.

`Token` will be a small data struct that combines a token `Kind` with the text that makes it up. You could also include other data like where in the program this token is located (which would be helpful in showing errors to someone writing html), but it's not strictly necessary here.

We define the type along with some initializers that'll help us as we're tokenizing.

```
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
```

Next, the `ScanningCursor` class will help us keep track of what character we're looking at at any given moment. This could theoretically just be a part of `Tokenizer`, but I've pulled it out into its own type for possible testability and to keep the purpose of the tokenizer simpler.

```
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
```

Finally, the `Tokenizer` itself:

```
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
```

The tokenizer's primary public function runs a loop, attempting to parse tokens until the cursor says we've reached the end of the program string.

The `scanNextToken()` method tells the cursor to pop off its next character and advance its internal position. With that `next` character, it then tries to decide what kind of token to make:

- if the character matches one of the punctuation token types, we append that token to our list and return
- if the character is whitespace, we add a single whitespace token
- otherwise, we assume the token will be some text, so we start scanning that.

`scanText()` grabs the most recently popped-off character and starts its own loop, accumulating text characters into a single string. Here we're considering "text" to be "anything that's neither whitespace nor one of our recognized punctuation tokens." This is a kind of strange way to tokenize text, but html is a strange kind of programming language! and we break things up this way to make parsing easier for us later on.

## The Parsing Context

As mentioned earlier, the **Parsing Context**, is a class whose core purpose is letting other types *consume* tokens they recognize, while also keeping track of which tokens have already been consumed. It's similar to the scanning cursor from earlier, but a little more tailored moving forward (and at times, backward) through a list of tokens.

You can think of this type as similar to a graphics context, like an OpenGL or Core Graphics context. A graphics context is kind of like a canvas, where you call drawing methods on it (stroke this path, fill this rectangle) or set properties (the current font, the current transform matrix, etc). These calls manipulate the internal state of the context, until you're ready for it to spit out a final rendered image.

The parsing context is kind of like that, but instead of *adding to an eventual image*, we're subtracting bits of the internal token state while we parse out types. When we'er all done parsing, the context should ideally be at the end of its list of tokens and we should have all our parsed data.

```
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
	
	// These might be whitespace tokens.
	var currentToken: Token { isNotAtEnd == false ? tokens.last! : tokens[currentTokenIndex] }
	var nextToken: Token { tokens[currentTokenIndex + 1] }
	var nextNextToken: Token { tokens[currentTokenIndex + 2] }
	var previousToken: Token { tokens[currentTokenIndex - 1] }
	
	init(tokens: [Token]) {
		self.tokens = tokens
	}
	
	enum ParseError: Error {
		case unexpectedToken(Token, feedback: String)
		case failedToParse
	}
```

We start with some properties around accessing the tokens. We store a list of all tokens and access them by an index, which is our current parsing location. Instead of storing a single index, we instead have a stack of indexes, with the *current* index being the top of this index stack. We'll look into this more below, but it allows us more flexibility as we're parsing.

```	
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
```

Next, we have the primary methods used for updating the token state, by advancing the cursor when we finding (`advance(when:...)`) and consuming matching tokens. The `advance()` method more or less just checks to see if the given `predicate` closure matches the current token. Most of the time in programming languages, we ignore whitespace tokens, so this method does that by default, but it has a flag to not skip, since we'll need that later on for some of our parsing.

The `consume(...)` methods build upon `advance(...)`, but `throw` an error if matching fails. From this point onward in the parser architecture, we use Swift errors as a means of control flow to indicate more or less that parsing a certain token or syntax node was unsucessful. This doesn't necessarily mean there is an error, only that we weren't able to interpret a specific part a certain way (it might mean it should be interpreted another way).

```
	// MARK: - Helpers
	
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
	
	@discardableResult
	func consumeBetween<ContentType>(leftToken: Token.Kind, rightToken: Token.Kind, content: () throws -> ContentType) throws -> ContentType {
		try consume(tokenKind: leftToken, feedback: "Expected a \(leftToken)")
		let consumedContent = try content()
		try consume(tokenKind: rightToken, feedback: "Expected a \(rightToken)")
		
		return consumedContent
	}
}
```

Finally, we have 4 helper methods that we'll use while parsing.

`untilThrowOrEndOfTokensReached(perform:)` calls its `perform` closure in a loop, accumulating values returned from it in an array, which it eventually returns either when the end of tokens is reached or (more likely) when the closure throws an error. In practice, we'll be calling other methods of the parsing context inside that closure while parsing syntax nodes. The point of this method is to essentially say "after a while, parsing failed, so I'm gonna give you what I've successfully parsed until that point."

`attempt(action:)` is extremely useful, as it allows us to try multiple parsing actions in the hopes they succeed (and thus, move the token cursor forward), but if the action `throws`, we're able to revert back to the previous cursor position. If we didn't use `attempt(action:)` when parsing and e.g., called `consume()` twice successfully, and then a third time unsucessfully, we would have failed to parse a whole thing, but we would have also moved the cursor along with us, now in a spot unable to try finding something else. `attempt(:)` solves this for us, and is why we use a stack of token indexes instead of just a single index (this also works recursively).

`choose(from:)` takes an array of closures with parsing calls in them, each returning a value. It then runs through the array, calling each closure in order. If a closure successfully returns a value, `choose` will return that value. If a closure `throws`, then we move on to the next closure to try that. All of this is wrapped in an `attempt(action:)` call so that if parsing in one closure fails, the next one gets a fresh start before it parses. This method is useful when parsing could result in multiple possibilities in the same place in the program, and you frequently (but not always) would want your return type to be an `enum` with a choice for each of its cases.

Finally, `consumeBetween(leftToken:,rightToken:, content:)` helps us in the case when things are wrapped in certain tokens, for example quotes, parentheses, or angle brackets. It tries to consume the left token, then tries the `content` closure, and finally tries to consume the right token. If all of that succeeded, it returns whatever was returned by the closure.

And that completes the `ParsingContext`, which is models common operations used throughout the HTML parsing process (and which could easily be reused with parsers for your own programming language too).

## Parsing HTML

Now that we've built ourselves parsing tools, lets use them to parse out HTML into our own data types (in programming language theory, these are known as "abstract syntax trees / nodes," which is a fancy way of saying a set of types that are usually arranged in some sort of hierarchy or graph). We'll only make use of a few types, as most of HTML is fairly generic and has a similar structure all the way down.

To identify these syntax tree nodes, let's make a protocol for anything that is `Parsable`:

```
protocol Parsable {
	static func parse(context: ParsingContext) throws -> Self
}
```

Types that conform to this protocol will have to implement the above static method and return a parsed version of themselves, or throw an error if they couldn't be parsed out of the given `ParsingContext`. You could alternatively make this an initializer method instead, but that will shadow the auto-generated `struct` initializers, which is kind of annoying.

### Document

Let's start at the top, with the `Document`. An HTML document is our model that more or less lines up with the html "file" as a whole. We'll keep ours very simple:

```
struct Document: Hashable, Parsable {
	
	enum DocumentError: Error {
		case unableToFindHTMLNode
	}
	
	let htmlNode: Node
	
	static func parse(context: ParsingContext) throws -> Document {
		let nodes = context.untilThrowOrEndOfTokensReached {
			try Node.parse(context: context, options: nil)
		}
		
		guard let htmlNode = nodes.first(where: { $0.element.lowercased() == "html" }) else {
			throw DocumentError.unableToFindHTMLNode
		}
		
		return Document(htmlNode: htmlNode)
	}
}
```

An html document has 0 or more "nodes" (tags) at the top level. It might have a `<!doctype>` tag, and it ideally should have an `<html>` tag too. Our `Document.parse(:)` implementation asks the given parsing context to parse out `Node`s until it an error is thrown or we've reached the end of the tokens. Then, we search through that array of nodes, looking for the `html` node, and finally, we return the document initialized with that found node (and we ignore any doctype or other nodes we might find). If we can't find any html node, we throw an error indicating such. It might be that the program really didn't contain an html tag, or more likely, that our `Node` parser failed to handle something inside the html node and errored out.

Our parser system is going to be kind of strict in what it accepts, which is contrary to how the Big Browsers tend to work, where they'll accept pretty much anything you throw at them. Our approach favours simplicity of implementation to get concepts across, at the cost of compatibility with lots of websites. As you build out your browser, feel free to expand what your parser can handle :)

### Node

Now it's time for the real meat and potatoes of our syntax tree, the `Node`, which represents a "node" in the html document. It's more or less the data model equivalent of a `<tag>`, any attributes inside of the tag itself, and any children nested between the tags (the distinction between a node, an element, and a tag is subtle, and you may be used to using the terms interchangeably, but I'll try to keep them separate as best I can).

Let's start the `Node` type with some internal types and properties:

```
struct Node: Hashable, Parsable {
	
	struct InternalElement {
		static let textRun = "__textRun"
		static let comment = "__comment"
	}
	
	enum Content: Hashable {
		case text(String)
		case childNodes([Node])
		case voidNode
	}
	
	enum NodeParseError: Error {
		case closingTagDidNotMatchOpeningTag(opening: String, closing: String)
		case openingTagWasActuallyClosing(tagName: String)
		case closingTagWasActuallyOpening(tagName: String)
		case didNotFindAnyText
	}
	
	let element: String
	let content: Content
	let attributes: [Attribute]
```

`InternalElement` lists some private element names we'll use for bits of the html file that don't fall under normal html tag rules (we'll see more of them later).

Then we have the `Content` enum, which models the stuff inside of our node. This says, a node can either contain text, child nodes, or be a "void" node (that is, a node that only has a start tag, no end tag and no children. `<img>` is an example of a void node).

Next, we have an error type defined to list the things that can go wrong during parsing and which act as control flow.

Finally, we have `Node`'s properties: its element (or tag name), the aforementioned content, and any attributes that were in the start tag.

Now it's on to parsing the node itself, which we'll break down into some chunks:

```
static func parse(context: ParsingContext) throws -> Node {
	let startTag = try Tag.parse(context: context)
	
	guard startTag.isEnd == false else {
		throw NodeParseError.openingTagWasActuallyClosing(tagName: startTag.element)
	}
	
	if startTag.element.lowercased() == "doctype" {
		return Node(element: "doctype", content: .voidNode, attributes: startTag.attributes)
	}
	
	if startTag.isVoidElement {
		return Node(element: startTag.element, content: .voidNode, attributes: startTag.attributes)
	}
```

We begin by trying to parse a start tag (which we'll get to in a bit). Then, we check some conditions to see if we can bail early:

- If the tag that got parsed was an end tag (eg `</something>`), then we throw an error. Alternatively, we could break `Tag` into 2 types, `StartTag` and `EndTag`, and let start tags fail to parse end tags.
- Then we check to see if our start tag is a `doctype` tag, in which case we return immediately.
- Finally, we check to see if the start tag represents a void element, and if so we also return immediately.

If none of those conditions are met, we keep parsing. At this point, we have a start tag and we need to look for 0 or more children we might have, before reaching an end tag.

To parse child nodes, we're going to ask the context to parse until we hit an error. This way, we'll get 0 or more child nodes. Inside that loop, we're going to ask the context to choose from a few possibilities:

```
	let children = context.untilThrowOrEndOfTokensReached(perform: {
		try context.choose(from: [
			{ try Node.parse(context: context) },
```

The child might be a normal `Node` of some kind, so we recursively call `Node.parse()`.

```
{
	let textContents = context.untilThrowOrEndOfTokensReached {
		try context.consume(where: { $0.kind != .openAngleBracket }, skipWhitespaceTokens: false, feedback: "Expected a non `<` token")
	}
	guard textContents.isEmpty == false else {
		throw NodeParseError.didNotFindAnyText
	}
	
	let contentRun = textContents
			.map(\.body)
			.joined()
			.replacingOccurrences(of: "&#x000A;", with: "")
			.replacingOccurrences(of: "\n", with: " ")
			.replacingOccurrences(of: "\t", with: " ")
			.replacingOccurrences(of: "&lt;", with: "<")
			.replacingOccurrences(of: "&gt;", with: ">")
			.replacingOccurrences(of: "&quot;", with: "\"")
			.replacingOccurrences(of: "&amp;", with: "&")
	
	return Node(element: InternalElement.textRun, content: .text(contentRun), attributes: [])
},
```

If it's not a standard node, it might be a **text run** node. Text runs in html are not real nodes like `<div>` or `<p>`, instead they're the any text content inside of other tags. So if we have a node like `<p>Hi there</p>`, this will get parsed out to a p `Node`, whose `content` is `.childNodes(children)`, and `children` will be an array with a single `Node`, whose `content` is `.text("Hi there")`. This structure *feels weird*, but it allows us to parse more complicated nodes like `<p>Hi there, <bold>friend</bold></p>`. In short, we're wrapping otherwise un-tagged text into a pretend `<text-run>` tag and then treating it as we do other nodes.

To parse a text run, we first consume every token that's not an `<` character, which we assume might be the beginning of a tag. If we find any contents, we then join the contents' body together into one big string.

Then, we do some quick and dirty text replacement, replacing encoded html entities with their display characters and non-space whitespaces with spaces for display (this doesn't follow the html standard for whitespaces perfectly, but it works well enough). With all the replacement done, we return the text run node.   

```
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
}])})
```

Finally, if the child node wasn't a normal node, nor a text run node, we see if it was perhaps a comment node, which takes the form `<!-- your comment here -->`. Looking inside angle brackets, we first attempt to consume a bang, then 2 hyphen tokens. After that, we loop, peeking at the next 3 tokens looking for the ending `-->` pattern. If we don't find that pattern, we just consume and ignore whatever content was there. Once we're done munching tokens, we return the internal comment node.

```
		.filter {
			if $0.element == InternalElement.comment { return false }
			if $0.element != InternalElement.textRun { return true }
			
			// filter out empty text run nodes
			return $0.textContent?.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty == false
		}
```

As a very last step of parsing child nodes, we remove nodes that are comments or nodes that are text runs with empty text. Everything else, we keep. And now we're done parsing child nodes.

```
	
	let endTag = try Tag.parse(context: context)
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
```

After the child nodes are parsed, all that's left is to parse the end tag, make sure it's really an end tag, and ensure that it matches the start tag. If all of that succeeded, we return the fully constructed `Node`. Most of what we just did was bookkeeping (checking tags, make sure start / end tags match), and then parsing the node's children, if any.

### Tag

We've papered over `Tag` parsing, though, so let's look at that now:

```
struct Tag: Parsable {
	
	let element: String
	let isEnd: Bool
	let attributes: [Attribute]
	
	var isVoidElement: Bool {
		["area", "base", "br", "col", "embed", "hr", "img", "input", "link", "meta", "source", "track", "wbr"].contains(element)
	}
```

We start our `Tag` type with some properties, alluding to the `Attribute` type we'll see shortly as well. We also list the known void elements to determine if our element should be considered void. Now on to the parsing:

```	
	static func parse(context: ParsingContext, options: ParsingOptions?) throws -> Tag {
		try context.consumeBetween(leftToken: .openAngleBracket, rightToken: .closeAngleBracket) {
			let slashToken = try? context.consume(tokenKind: .forwardSlash, feedback: "Expected a `/`")
			let _ = try? context.consume(tokenKind: .bang, feedback: "Expected a `!`")
```

A tag is wrapped in `<` and `>` angle brackets. Within those, we first look for an initial forward slash token, and if we find it we assume we're parsing an end tag (we use `try?` to optionally parse this — if we don't find the slash, we're not considering that an error worth bailing from). We also look for an optional bang token and just completely ignore it if we find it (this is for the `<!doctype>` tag).

```
			let identifier = try context.consume(tokenKind: .text, feedback: "Expected a tag name")
			
			let attributes = context.untilThrowOrEndOfTokensReached(perform: {
				try context.attempt(action: {
					try Attribute.parse(context: context, options: options)
				})
			})
```

Next, we parse an identifier that we'll use for the tag's element. Then we attempt to parse as many attributes as we can (there may be 0).

```
			// If there's a trailing slash (eg <img />), consume it but ignore it. this is invalid html
			_ = try? context.consume(tokenKind: .forwardSlash, feedback: "Expected a trailing `/`")
			return Tag(element: identifier.body, isEnd: slashToken != nil, attributes: attributes)
		}
	}
}
```

Finally, optionally look for and ignore a trailing slash at the end of the tag, as it's not actually valid html (this news to me when I started working on the browser). However, it's extremely common, so I thought it warranted handling here to make more of the web work. With that out of the way, we return our completed tag.

### Attribute

Ok, last part of the parser! the lil attributes inside a tag.

```
struct Attribute: Hashable, Parsable {
	let key: String
	let value: String
	
	enum AttributeParseError: Error {
		case emptyAttributeValue(key: String)
	}
	
	static func parse(context: ParsingContext, options: ParsingOptions?) throws -> Attribute {
		
		let key = try context.consume(tokenKind: .text, feedback: "Expected an attribute name")
		
		guard let _ = try? context.consume(tokenKind: .equals, feedback: "Expected an equals sign") else {
			return Attribute(key: key.body, value: key.body)
		}
```

Attributes are (usually) key-value pairs, so those are our properties (attributes that don't have explicit values, we'll just repeat the key for the value). 

Then, we start parsing. First we parse the key, then we look for an equals sign token. If we don't find it, we assume this attribute is the valueless kind and return it immediately. Otherwise, we parse the value, as a choice:

```
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
```

First choice: the value is between double quotes, and we consume everything inside that isn't a double quote (and we don't skip whitespaces either). Then we join all those tokens together and return that as the value.

```
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
```

Second choice: same thing as before, except between single quotes.

```
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
```

Final choice: we look for a value that's *not* wrapped in any kind of quotes. These kinds of values are delimitted by whitespace (or an angle bracket), so we consume basically everything else, make sure we actually found something non-empty, and join those tokens together into a value.

```
		])
		
		return Attribute(key: key.body, value: value)
	}
}
```

Last, we return the completed attribute.

## End of Part 1

This completes the end of part 1! We built ourselves some tools for breaking apart a program string into tokens and parsing them. And then we built some data types that know how to parse themselves using those tools. HTML is a kind of strange language, but we saw some familiar patterns repeated in multiple places (things being wrapped inside others, for example).

In the next part, we'll take the data we just parsed and render it with SwiftUI.
