#  Let's Write a Web Browser from Scratch in Swift!

There's a rumour that Apple's going to start allowing custom, non-WebKit based browser engines on iOS starting later this year. While that most likely means Chrome, Firefox, and the other big browsers could start using custom engines, it also means you could write your own too. So why not try it?

In this 2 part series, I'll take you through how to write a basic web browser, from parsing HTML in Swift to rendering the pages with SwiftUI, displaying them with a simple, but familiar interface.

<img src="smol-screenshot.png" style="display: block" width="600">

You might be thinking "Aren't web browsers huge, incredibly complicated pieces of software?" and yes, the big ones we use every day are huge and complicated. But even huge and complicated pieces of software are still "just software" at their core, written by normal programmers just doing their job or following their passion.

What we're attempting in this series is a very simple browser, and the end result is actually a little under 1000 lines of fairly straightforward Swift code. We'll focus solely on rendering a subset of HTML, leaving CSS and Javascript as exercises for the reader :). We'll take many shortcuts and liberties, but in the end you should have an app that can render unstyled, standard HTML pages. And you'll also have some tools for writing programming language parsers by hand, which you could use to write your own custom language.

The feature set of our browser is going to be small, but the goal is this: **you should be able to render this very browser tutorial web page in the browser itself**. Fun, right?

## The Architecture

Before we dive in to code, let's look at the overall archicture of what we're building, to make the challenge ahead more managable. Since HTML is a programming language, we'll follow a similar architecture to that of most compilers / interpreters, which is a sort of pipeline, where each part of the pipeline takes input and spits something else out for the next component to work with. So what are our inputs and components?

1. We start with the **raw html**, as a `String`. This either comes from the network or a local file, but it doesn't really matter.
2. We then digest the html string into an array of **tokens**, in a process known as *tokenizing* or *lexing*. This essentially chews up the raw string into common pieces that are easier to digest, such as punctuation characters (`<, >, ", etc`), whitespaces (newlines, tabs, spaces), digits, or just regular letter characters.
3. The tokens are then passed to the **Parsing Context**, a class whose core purpose is letting other types *consume* tokens they recognize, while also keeping track of which tokens have already been consumed.
4. Next we have structs representing **the data we're parsing**, things like, the `Document`, a tree of `Node`s, which consist of `Tag`s and `Attribute`s. We'll write little parsers for each of these things, that will call into the `ParsingContext` to consume the tokens they need for their construction.
5. Finally, when parsing is complete, we have data we can then use to **display our SwiftUI pages** with. In a traditional programming language, this might be the point where you output compiled code into an executable or evaluate your data with an interpreter, but here our "interpreter" will simply display a UI.

With that general archictecture in mind, let's fire up Xcode and get started.

### Quick Tips

If you're coding along as you read this tutorial, I highly recommend typing out all the code yourself, instead of copying and pasting. In my experience, I find this forces you to slow down and work more deliberately, and I think it'll help you understand things better in the process.

I'd also recommend changing things as you move along. None of what I've written is the definitive way to write this code, and you could probably put your own spin on it. Or extend it to do even more!

The code I'll be showing in this tutorial is more or less "finished" as is (we won't be building to much of it iteratively, because that would take up a whole book!), but please know my browser *was* built iteratively (you can check out the git history if you'd like to see my stumbles as I went!). Some bits of code in the tutorial will depend upon code we haven't written yet, so please use your imagination if things don't compile at every stage.

Finally, I won't be providing any unit tests in the tutorial, but you may very well like to include some, especially if you decide to extend your browser after you're done. I find programming languages lend themselves very well to unit testing, as they have well defined inputs and outputs.

## Starting the project

Create a new Xcode project using a SwiftUI template. I called my browser `Smol` because it's very tiny, but feel free to let your creativity shine here. I made my browser be a Mac app just for ease of playing around with, but you could make yours an iOS app if you wanted, everything will work more or less the same. 

In your project settings Info tab, add key for "App Transport Security Settings," and inside that add a key for "Allow arbitrary loads," setting its value to Yes. This will let us load http and https content from anywhere on the internet and it's not enabled by default.

## Tokenizing

Tokenizing is the process of breaking down our program from a `String` into an array of `Token` elements, by scanning through the program character by character to build up different tokens. We'll make 3 types: `Token`, `ScanningCursor`, and `Tokenizer`.

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

Next, the `ScanningCursor` class will help us keep track of what character we're looking at at any given moment. This could theoretically just be a part of `Tokenizer`, but I've pulled it out into its own type for possible testability and to keep the tokenizer simple.

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
	
	func scanAllTokens() -> [Token] {
		while cursor.isNotAtEnd {
			scanNextToken()
		}
		
		return scannedTokens
	}
	
	private func scanNextToken() {
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
- otherwise, we assume the token will be any other text, so we start scanning that.

`scanText()` grabs the most recently popped-off character and starts its own loop, accumulating text characters into a single string. Here we're considering "text" to be "anything that's neither whitespace nor one of our recognized punctuation tokens." This is a kind of strange way to tokenize text, but html is a strange kind of programming language! and we break things up this way to make parsing easier for us later on.

## The Parsing Context

As mentioned earlier, the **Parsing Context**, is a class whose core purpose is letting other types *consume* tokens they recognize, while also keeping track of which tokens have already been consumed. It's similar to the scanning cursor from earlier, but a little more tailored moving forward (and at times, backward) through a list of tokens.

You can think of this type as similar to a graphics context, like an OpenGL or Core Graphics context. A graphics context is kind of like a canvas, where you call drawing methods on it (stroke this path, fill this rectangle) or set properties (the current font, the current transform matrix, etc). These calls manipulate the internal state of the context, until you're ready for it to spit out a final rendered image.

The parsing context is kind of like that, but instead of *adding to an eventual image*, we're subtracting bits of the internal token state while we parse out types. When we're all done parsing, the context should ideally be at the end of its list of tokens and we should have all our parsed data.

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

We start with some properties around accessing the tokens. We store a list of all tokens and access them by an index, which is our current parsing location. Instead of storing a single index, we instead have a stack of indexes, with the *current* index being the top of this index stack. We'll look into this more below, but it allows us to move forward *and backward* through the list of tokens as we're parsing.

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

The `consume(...)` methods build upon `advance(...)`, but will `throw` an error if matching fails. From this point onward in the parser architecture, we use Swift errors as a means of control flow to indicate more or less that parsing a certain token or syntax node was unsucessful. This doesn't necessarily mean there is an error, only that we weren't able to interpret a specific part a certain way (it might mean it should be interpreted another way).

The consume method takes a feedback string to make parsing failures a little clearer, and to make bugs in the parser a little easier to track down.

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

Finally, `consumeBetween(leftToken:, rightToken:, content:)` helps us in the case when things are wrapped in certain tokens, for example quotes, parentheses, or angle brackets. It tries to consume the left token, then tries the `content` closure, and finally tries to consume the right token. If all of that succeeded, it returns whatever was returned by the closure.

And that completes the `ParsingContext`, which models common operations used throughout the HTML parsing process (and which could easily be reused with parsers for your own programming language too).

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

An html document has 0 or more "nodes" (tags) at the top level. It might have a `<!doctype>` tag, and it ideally should have an `<html>` tag too. Our `Document.parse(:)` implementation asks the given parsing context to parse out `Node`s until an error is thrown or we've reached the end of the tokens. Then, we search through that array of nodes, looking for the `html` node, and finally, we return the document initialized with that found node (and we ignore any doctype or other nodes we might find). If we can't find any html node, we throw an error indicating such. It might be that the program really didn't contain an html tag, or more likely, that our `Node` parser failed to handle something inside the html node and errored out.

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

To parse child nodes, we're going to ask the context to parse nodes  until we hit an error. This way, we'll get 0 or more child nodes. Inside that loop, we're going to ask the context to choose from a few possibilities:

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
					try context.consume(where: { _ in true }, skipWhitespaceTokens: false, feedback: "consuming comment contents")
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

Finally, optionally look for and ignore a trailing slash at the end of the tag, as it's not actually valid html (this was news to me when I started working on the browser). However, it's extremely common, so I thought it warranted handling here to make more of the web work. With that out of the way, we return our completed tag.

### Attribute

Ok, last part of the parser! the attributes inside a tag.

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

Attributes are (usually) key-value pairs, so those are our properties (for attributes that don't have explicit values, we'll just repeat the key for the value). 

Then, we start parsing. First we parse the key, then we look for an equals sign token. If we don't find it, we assume this attribute is the valueless kind and return it immediately. Otherwise, we parse the value, as a choice:

```
let value = try context.choose(from: [
{
	try context.consumeBetween(leftToken: .doubleQuote, rightToken: .doubleQuote) {
		let textContents = context.untilThrowOrEndOfTokensReached {
			try context.consume(where: { $0.kind != .doubleQuote }, skipWhitespaceTokens: false, feedback: "Expected a non quote token")
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
			try context.consume(where: { $0.kind != .singleQuote }, skipWhitespaceTokens: false, feedback: "Expected a non single quote token")
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


# Part 2: Rendering in SwiftUI

## The Architecture

The architecture of our rendering engine should look pretty familiar to anyone who's worked with SwiftUI before: we're more or less just going to have views which render our node hierarchy. It's almost exclusively composed of standard SwiftUI views, plus a controller object for loading HTML pages, and a few extensions on the `Node` type to more easily work with its properties. Here are the main pieces we'll be working with.

- `PageController` is responsible for loading web urls asynchronously and parsing them into `Document`s. It also maintains the back / forward stacks of documents.
- Some views:
	- `BrowserView` is the primary view, containing our chrome (back / forward / address bar) and the document view.
	- `WebDocumentView` displays either a homepage, error page, or the contents of the loaded page, depending on the page controller's state.
	- `BodyView` is the true beginnings of our rendering engine, it nests our page's content in a scroll view.
	- `BlocksView` displays views for 0 or more nodes in a vertical stack. It picks a different view depending on the node's element.
	- `InlineContentWrappingBlockView` combines the text of all its inline elements into one big `Text` for rendering.
	- `ListNodeView` renders ordered or unordered lists and their items.
	- `ImageView` asynchronously downloads and renders img nodes.
- Extensions on `Node` for accessing its content.

### The Page Controller

The `PageController` is our main controller object, responsible for loading pages, parsing them, and managing the back / forward stacks:

```
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
```

First we set up some nested types. The controller can be in one of three `State`s: an initial unloaded state (maybe you show a homepage?), a loaded state with the parsed document and the URL it came from, and the failed error state.

Then we have some properties, mainly the controller's current `state`, its current `address` string, and the back / forward stacks.

```
	func loadPage(at url: URL) {
		Task {
			let newState: State
			do {
				let (data, response) = try await URLSession.shared.data(from: url)
				
				if let currentlyLoadedDocument {
					backStack.append(currentlyLoadedDocument)
					forwardStack = []
				}
				
				let htmlString = String(data: data, encoding: .utf8) ?? ""
				let tokenizer = Tokenizer(programText: htmlString)
				let context = try ParsingContext(tokens: tokenizer.scanAllTokens())
				
				newState = .loaded(try Document.parse(context: context, options: nil), response.url ?? url)
			} catch {
				print("error loading page: \(error)")
				newState = .failed(error)
			}
			
			await MainActor.run {
				state = newState
			}
		}
	}
```

To load a page, we kick off an async `Task`, await the loading of the given url, then we put the data through our parser pipeline. We also set the back / forward stacks to account for the state change that's about to happen.

This is all made a little awkward due to error handling, as we want to catch any errors that happen here: there could be URL related errors, there could be an error in the parsing context, or there could be an error parsing the document. If there is an error, we want to record it. This wouldn't be so bad on its own, but we don't want to do any of this parsing on the main actor, where it could freeze the UI, *but* we must update our controller's `state` property on the main actor, as our view depends on that property to draw itself.

```
	private var currentlyLoadedDocument: (Document, URL)? {
		switch state {
		case .notLoaded, .failed: return nil
		case let .loaded(document, url): return (document, url)
		}
	}
	
	func goBack() {
		guard let (previousDocument, previousURL) = backStack.popLast() else { return }
		if let currentlyLoadedDocument {
			forwardStack.append(currentlyLoadedDocument)
		}
		state = .loaded(previousDocument, previousURL)
	}
	
	func goForward() {
		guard let (nextDocument, nextURL) = forwardStack.popLast() else { return }
		if let currentlyLoadedDocument {
			backStack.append(currentlyLoadedDocument)
		}
		state = .loaded(nextDocument, nextURL)
	}
}
```

Finally, we have a helper property for accessing the currently loaded document, if any, and methods for going back and forward. That wraps up our controller. Next, we'll see how the views make use of it while displaying our nodes.

### The Browser View

As mentioned above, our `BrowserView` is the primary view for our browser window: it composes the "chrome" of our UI, plus the actual rendered content in another view. Our UI is going to be very simple, but you could extend it to use tabs, or even something more imaginative if you want :)

```
struct BrowserView: View {
	@ObservedObject var controller: PageController
	@FocusState private var addressIsFocused: Bool
```

All we need are 2 properties, an observed page controller and the focus state of the address textfield, so that focus works like you'd expect as we navigate.

```
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
```

The body of our view until this point is all about the chrome. We create our back / forward buttons and the address bar, and we bind their actions to our controller.

```
			WebDocumentView(controller: controller)
				.background(.white)
				.environment(\.openURL, .init(handler: { url in
					controller.loadPage(at: fullURL(forURLToLoad: url))
					addressIsFocused = false
					return .handled
				}))
		}
```

We configure the WebDocumentView and override SwiftUI's `openURL` environment value. When the user clicks a link in our app, SwiftUI invokes this callback, giving our app a chance to handle the URL. With the given URL, we construct an absolute URL (below), adjust the text field's focus, and tell the system we handled the url (we could also tell the system to handle it instead if the URL was eg `mailto:...`, but I'll leave that to you). 

```
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
} // End of BrowserView

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
```

Finally, we use the environment modifier for a custom environment value. The `urlBuilder` is a closure / function responsible for taking a URL (one that's possibly relative, eg just `/page.html` vs `https://example.com/page.html`) and expanding it to an absolute URL so that pages and assets like images can be loaded.

We do this as an environment value so that other views in the hierarchy can access the functionality.

### Web Document View

The `WebDocumentView` takes up the majority of space in our browser window. What it shows depends on the `state` of the page controller, either showing a simple home page, error screen, or the loaded content.

```
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
```

The `BodyView` accesses some properties on `Node` which we'll write shortly for accessing child nodes more easily. We drill down to find the page's title, if it has one, and set that as our window title. Finally, we set a default font on the document's text. "Times" is the font you see in most browsers with unstylized text (but you're allowed to choose any font you'd like here).

### Node extensions

Before we go any further with our views, let's write those helpers in an extension on `Node`.

```
extension Node {
	var childNodes: [Node] {
		switch content {
		case .voidNode, .text: return []
		case .childNodes(let nodes): return nodes
		}
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
```

These properties help us access child nodes and text content more easily.

```
	var childNodesSortedIntoBlocks: [Node] {
		var nodesToReturn = [Node]()
		var inlineElements = [Node]()
		
		func addInlineElementsAsGroupIfNeeded() {
			guard inlineElements.isEmpty == false else { return }
			// make a fake block element that has all these as children
			let wrapper = Node(element: "p", content: .childNodes(inlineElements), attributes: [])
			// and append it to our list to return
			nodesToReturn.append(wrapper)
			// then, empty the inlineElements list
			inlineElements = []
		}
		
		for node in childNodes {
			if isInlineNode {
				inlineElements.append(node)
			} else {
				addInlineElementsAsGroupIfNeeded()
				nodesToReturn.append(node)
			}
		}
		addInlineElementsAsGroupIfNeeded()
		return nodesToReturn
	}
	
	var isInlineNode: Bool {
		[InternalElement.textRun, "a", "abbr", "acronym", "audio", "b", "bdi", "bdo", "big", "br", "button", "canvas", "cite", "code", "data", "datalist", "del", "dfn", "em", "embed", "i", "iframe", "img", "input", "ins", "kbd", "label", "map", "mark", "meter", "noscript", "object", "output", "picture", "progress", "q", "ruby", "s", "samp", "script", "select", "slot", "small", "span", "strong", "sub", "sup", "svg", "template", "textarea", "time", "u", "tt", "var", "video", "wbr"].contains(element)
	}
```

This next property is a little more involved. When we're rendering nodes, we want block nodes, like `<p>`, `<div>`, etc. to flow one after another, vertically down the page, while things like `<bold>`, `<a>`, etc. flow within the same line like words in a paragraph.

The trouble for us is, in html those inline elements don't have exist inside of block elements at all, they can exist outside of them too. For example:

```
<body>
	<bold>Some bold text</bold>
	<p>A paragraph</p>
</body>
```

The bold text is just kinda hanging out as inline, but inline relative *to what?* I'm not entirely sure how other browsers solve this, but we've solved it by grouping any inline elements as children of a fake, inserted `<p>` node.

```
	var attributeDictionary: [String: String] {
		Dictionary(uniqueKeysWithValues: attributes.map({ ($0.key, $0.value) }))
	}
}
```

Lastly, we offer a way to access the node's attributes as a dictionary.

Now we have enough tools at our disposal to write the rest of the views.

### The BodyView

This view hosts our browser's scroll view, which then displays child nodes in another view.

```
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
```

The hierarchy here is mostly straightforward: the `BlocksView` is initialized with the child nodes of the body and is given a global padding. Then we extend the frame of the scroll view to stretch as much as possible and align the content to the top leading edge, like other browsers do.

### BlocksView

This one is kind of fun: it's a reusable view that vertically stacks the child nodes it was given, rendering them with the appropriate view depending on what element they are. It even recursively uses itself in a few cases.

```
struct BlocksView: View {
	let children: [Node]
	
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
						.font(Font.system(size: 13, design: .monospaced))
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
```

We don't support special rendering for every element under the sun, so if we find an element we don't know about, we just render that we found an unknown block. You could default it to behaving like a `<div>` if you wanted, but I like calling them out like this instead because I'm more motivated to give it a proper view that way.

### Inline nodes

Inline nodes are interesting, because to render them we can't just use views placed in some kind of stack. Instead, we want them to be rendered one after another like text, wrapping to the next line as needed. And indeed, that's how we're going to do it in SwiftUI, by combining (or in Swift terms, using `reduce()`) inline contents into an `AttributedString` and rendering it in a single `Text` view per inline "block."

```
struct InlineContentWrappingBlockView: View {
	let node: Node
	@Environment(\.font) var font
	
	var body: some View {
		Text(
			node
				.childNodes
				.map { $0.attributedText(defaultFont: font ?? Font.custom("Times", size: 16)) }
				.reduce(AttributedString(), +)
		)
		.lineSpacing(4)
		.fixedSize(horizontal: false, vertical: true)
	}
}
```

In the body of our body, we return a single `Text`, initialized with an attributed string. The attributed string is created by mapping the node's child nodes and calling the `attributedText(defaultFont:)` method on each (we'll see that property in a moment). This mapping gives us an array of attribute strings, so we `reduce()` them into a single attributed string.

```
extension Node {
	func attributedText(defaultFont: Font) -> AttributedString {
		switch element {
		case InternalElement.textRun:
			var attributes = AttributeContainer()
			attributes.font = defaultFont
			
			return AttributedString(textContent ?? "", attributes: attributes)
```

To get the attributed text for a node, we switch over its `element` to see how we should format it. Here we have the base case: a text run. We create an attribute container, use the font that was passed in, and return an attributed string with the node's text content and those attributes.

```
		case "em", "i":
			var attributes = AttributeContainer()
			attributes.font = defaultFont.italic()
			
			return childNodes
				.map { $0.attributedText(defaultFont: defaultFont.italic()) }
				.reduce(AttributedString(), +)
				.mergingAttributes(attributes, mergePolicy: .keepCurrent)
```

The rest of the cases are similar, in that we create some attributes, modifying the passed in font as needed. But in order to create the final attributed string, we actually need to recursively call ourselves so that we can handle multiple overlapping styles (eg a link node wrapped inside an italics node). 

```
		case "strong", "b":
			var attributes = AttributeContainer()
			attributes.font = defaultFont.bold()
			
			return childNodes
				.map { $0.attributedText(defaultFont: defaultFont.bold()) }
				.reduce(AttributedString(), +)
				.mergingAttributes(attributes, mergePolicy: .keepCurrent)
		case "code":
			var attributes = AttributeContainer()
			let monospaced = Font.system(size: 13, design: .monospaced)
			attributes.font = monospaced
			
			return childNodes
				.map { $0.attributedText(defaultFont: monospaced) }
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
			var attributes = AttributeContainer()
			attributes.font = defaultFont
			
			return childNodes
				.map { $0.attributedText(defaultFont: defaultFont) }
				.reduce(AttributedString(), +)
				.mergingAttributes(attributes, mergePolicy: .keepCurrent)
		}
	}
}
```

It's all a little boilerplatey but it gets the job done.

### ImageView

SwiftUI already has the a perfect view for us: `AsyncImage`, which we'll wrap in our own `ImageView` to customize it a little.

(Note: `<img>` nodes still won't render yet because they're considered inline by default, and we won't add support for inline images. However, at the end of the tutorial we'll add support for `display: block` styles on images)

```
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
```

From the environment, we pull out the `urlBuilder` function we declared earlier in our view hierarchy, so that we can make sure the image's `src` url is an absolute url, that we'll then hand off to SwiftUI to load asynchronously for us. When the image is ready, we resize it and constrain it as necessary, depending on any width or height attributes of the `<img>` node.

```
struct WebSize {
	let rawValue: String
	
	var dimension: CGFloat {
		// trim anything that isn't a digit, then try to parse that into an int. this ignores things like "px"
		CGFloat(Int(rawValue.prefix(while: \.isWholeNumber)) ?? 0)
	}
}
```

`WebSize` is a small little type for extracting number values out of sizing values in html. We're assuming everything is measured in `px` for simplicity's sake. Sizing in html is a complicated topic, but you could go deep here if you wanted.

### ListNodeView

Our last node view is the `ListNodeView`, which we'll use for displaying both ordered and unordered lists (`<ol>` and `<ul>`).

```
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
```

We start with an enum for the two list styles, and a function for picking the right list item marker for the given index.

```
	let node: Node
	let style: Style
	
	var body: some View {
		VStack(alignment: .leading, spacing: 8) {
			ForEach(Array(zip(node.childNodes.indices, node.childNodes)), id: \.1) { (index, childNode) in
				HStack(alignment: .firstTextBaseline, spacing: 8) {
					Text(verbatim: style.listMarker(for: index))
					BlocksView(children: childNode.childNodesSortedIntoBlocks)
				}
			}
		}
	}
}
```

The body of the list node should look pretty straightforward at this point: a vertical stack wherein we iterate all our child nodes and render them as list items (using a kind of noisy `Array/zip` dance to get the index). A list item uses a horizontal stack to display the marker, followed by a `BlocksView` for the grouped children (it could be, for example, multiple paragraphs).

### Preserving Whitespace

You may notice at this point, if you load up a webpage with a `<pre>` tag, it uses a monospaced font but it does not preserve whitespace. That's because we're currently discarding tabs and newlines in our parser. But for `<pre>` tags we should be preserving it in child nodes (and all of their descendents). So let's make some modifications to the parser.

In the parse method of `Node`, immediately before parsing child nodes, let's add the following line:

```
let shouldPreserveWhiteSpace = startTag.element == "pre"
```

Then, in the 2nd choice closure (text runs), we want to replace whitespaces only when we're *not* preserving whitespace. Change our entity decoding code to the following:

```
let entityDecodedContents = textContents
	.map(\.body)
	.joined()
	   .replacingOccurrences(of: "&#x000A;", with: "")
	   .replacingOccurrences(of: "&lt;", with: "<")
	   .replacingOccurrences(of: "&gt;", with: ">")
	   .replacingOccurrences(of: "&quot;", with: "\"")
	   .replacingOccurrences(of: "&amp;", with: "&")

let contentRun = shouldPreserveWhitespace ? entityDecodedContents : entityDecodedContents
	.replacingOccurrences(of: "\n", with: " ")
	.replacingOccurrences(of: "\t", with: " ")
```

This preserves whitespace great when the text is a direct child of the node, but doesn't yet handle deeper nestings. To do that, we'll have to pass `shouldPreserveWhitespace` as a flag to child node parsing. To keep things simple, let's add the flag to our `Parsable` protocol requirement:

```
static func parse(context: ParsingContext, shouldPreserveWhitespace: Bool) throws -> Self
```

After you make the change you'll have to go through all the call sites where we implement the `parse(...)` method and update them to include the new flag. At pretty much every callsite, just give a value of `false`. However, let's return to `Node`, specifically where we're parsing a child node (the first choice). Change the parse call to:

```
try Node.parse(context: context, shouldPreserveWhitespace: shouldPreserveWhitespace)
```

so that we can pass it down the line. Finally, where we declare our local variable for preserving whitespace, we update that to factor in the parameter passed in:

```
let shouldPreserveWhitespace = startTag.element == "pre" || shouldPreserveWhitespace
```

Now if we run the browser, we should see that whitespace preservation works as expected.

### Two Last Things

You may have noticed that most browsers, when given an unstyled html page, will render the body using the entire width of the browser window, and our browser does this exact same thing. However, on modern monitors, this can result in extremely long lines of text that are kind of hard to read due to their length, so the nice thing to do is to style the container with a maximum width.

While supporting all of CSS is way, way out of scope for this tutorial, it would still be nice to leave us with a starting point, so I'd like to add support to the `style` attribute, which we'll parse in a rather crude way. Let's start with a type representing a style.

```
struct Style {
	
	var maxWidth: CGFloat? {
		rawValue["max-width"].map(WebSize.init(rawValue:)).map(\.dimension)
	}
	
	private let rawValue: [String: String]
	
	init(rawPairs: [String: String]) {
		self.rawValue = rawPairs
	}
}
```

`Style` wraps an underlying dictionary of keys and values, and adds a helper property that looks for a `max-width` key and returns the value interpreted as a float. Now let's create a style instance from a node's style attribute, if it exists. In an extension on `Node`, put the following:

```
var styleFromAttributes: Style? {
	guard let styleAttribute = attributeDictionary["style"] else { return nil }
	let stylePairs = styleAttribute.components(separatedBy: ";")
```

First, we check to see if we even have a style attribute, otherwise we bail. Then, we break up the value string into substrings, which are separated by a semicolon.

```
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
```

Finally, we initialize the `Style` with a dictionary, whose keys and values are found by splitting up those substrings from earlier on colons, trimming out whitespace, and finally returning them as a non-nil tuple. This code is kinda fragile would definitely be made more powerful (and extensible!) if we wrote a parser like we did for html, but I'll leave that as an exercise for the reader :)

Now that we can access a typed version of a node's style attribute, let's use it on our `BodyView`. Change the body view's `body` (:S) scroll view to this:

```
ScrollView {
	HStack(spacing: 0) {
		BlocksView(children: bodyNode.childNodesSortedIntoBlocks)
			.frame(maxWidth: bodyNode.styleFromAttributes?.maxWidth)
		Spacer()
	}
	.padding(20)
}
```

This makes the `BlocksView` respect the `max-width` from the `<body>` tag, if it exists.

Ok, last last last thing I promise: in html `<img>` nodes are considered inline elements by default. Our browser doesn't support this at all (but you could add it if you'd like). We *do* want to support images as block elements, though. Now that we have basic support for the style attribute, let's extend it to support the `display` property. Inside `Style`, add the following:

```
enum DisplayStyle { case inline, block }

var display: DisplayStyle? {
	switch rawValue["display"] {
	case "inline": return .inline
	case "block": return .block
	default:
		return nil
	}
}
```

Then, in our `childNodesSortedIntoBlocks` property, edit the the for loop to look like this:

```
for node in childNodes {
	let defaultDisplayStyle: Style.DisplayStyle = node.isInlineNode ? .inline : .block 
	let display = node.styleFromAttributes?.display ?? defaultDisplayStyle
	if display == .inline {
		inlineElements.append(node)
	} else {
		addInlineElementsAsGroupIfNeeded()
		nodesToReturn.append(node)
	}
}
```

With that modification, `<img>` nodes that have a style attribute declaring they should be `display: block` will now be properly considered block views in our renderer, and appear accordingly.

## The End

This concludes our browser engine. We wrote a simple html parser from scratch with Swift, and then wrote a rendering engine using SwiftUI. In all, the browser should be capable of rendering this very tutorial, and it should look pretty much identical to how it looks in Safari, Chrome, or other Big Browsers.

You could extend this foundation in so many ways:

- You could add more block or inline elements types
- You could expand what style elements are supported
- You could even write your own CSS parser! or if you have limitless ambition, you could write a javascript engine too

But most of all, I hope you enjoyed yourself and learned a thing or two.

Thanks for reading!

(ps: I'm looking for work, so if you're looking to hire someone to work on browsers, programming languages, dev tools, or Swift apps, I'm your guy! [Please reach out](mailto:i.jasonbrennan@gmail.com), I'd love to hear from you)

todo:

- screenshots
