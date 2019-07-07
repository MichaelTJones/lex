package lex

import (
	"bytes"
	"fmt"
	"strings"
	"unicode"
	"unicode/utf8"
)

// Position of token in input
type Position struct {
	Filename string // filename, if any
	Offset   int    // byte offset, starting at 0
	Line     int    // line number, starting at 1
	Column   int    // column number, starting at 1 (character count per line)
}

func (p Position) String() string {
	switch {
	case p.Filename == "":
		return fmt.Sprintf("%d:%d", p.Line+1, p.Column+1)
	default:
		return fmt.Sprintf("%s:%d:%d", p.Filename, p.Line+1, p.Column+1)
	}
}

// simple lexical scanner
type Lexer struct {
	Input []byte

	// details of the most recently parsed token
	Value   string // token value as a string: "0xfade"
	Type    Token  // token type: lexNumber
	Subtype Token  // token subtype: lexHexadecimal [Number:Integer:Hexadecimal]
	Chars   int    // size of token in code points
	Bytes   int    // size of token in bytes

	// Details of the most recently parsed token's location in the input
	Position

	// Bit mask indicating which types and subtypes to match or skip
	Mode uint // defaults to scan nothing, skip nothing, pass characters through

	// Do we want to limit letters in identifiers to be ASCII-only?
	ASCII bool // defaults to false

	input []byte // original "non-shrinking" input
}

//go:generate stringer -type=Token
type Token int

const (
	_          Token = -iota
	Character        // unmatched input characters (returned as themselves)
	Comment          // block or end-of-line comments
	Identifier       // [a-zA-Z][a-zA-Z0-9]*
	Keyword          // reserved keywords: if, for, func, go, ...
	Operator         // "+-*/..."
	Rune             // rune literal 'a', '\U00101234'
	Space            // non-printing whitespace: space, newline, return, tab
	String           // doublequote or unapostrophe delimited character strings
	Type             // predeclared type identifiers: bool, int, float64, map, ...
	Other            // other predeclared identifiers: iota, nil, new, true,...
	Number           // integers in various bases, floating point, rational, complex
	EOF              // end of input stream
)

// optimization:
//    replace (lex.Type == Keyword && !lex.mode(SkipKeyword))|| ...
//    with  !lex.mode(skipToken[-lex.Type])
var skipToken = []uint{
	0,
	SkipCharacter,
	SkipComment,
	SkipIdentifier,
	SkipKeyword,
	SkipOperator,
	SkipRune,
	SkipSpace,
	SkipString,
	SkipType,
	SkipOther,
	SkipNumber,
}

// comment subtypes
const (
	_     = iota // skip 0 so that (Subtype==0) will mean unspecified
	Block        // block comments ("/* ... */")
	Line         // end-of-line comments ("// ... \n")
)

// identifier subtypes
const (
	_       = iota // skip 0 so that (Subtype==0) will mean unspecified
	ASCII          // identifier with ASCII-only letters
	Unicode        // identifier with arbitrary Unicode letters (category L)
)

// operator subtypes
const (
	_            = iota // skip 0 so that (Subtype==0) will mean unspecified
	OperatorGo          // Go operators
	OperatorMath        // extra operators (exponentiation and math symbols)
)

// string subtypes
const (
	_     = iota // skip 0 so that (Subtype==0) will mean unspecified
	Quote        // doublequote delimited character string (`" ... "`)
	Raw          // unapostrophe delimited character string ("` ... `")
)

// integer subtypes
const (
	_           = iota // skip 0 so that (Subtype==0) will mean unspecified
	Binary             // binary integer with prefix ("0b" or "0B")
	Octal              // octal integer with prefix ("0o" or "0O") or leading zero ("0377")
	Decimal            // decimal integer
	Hexadecimal        // hexadecimal integer with prefix ("0x" or "0X")
	Floating           // floating point
)

const (
	ScanCharacter = (1 << iota)
	SkipCharacter

	ScanBlock // comment
	SkipBlock

	ScanLine // comment
	SkipLine

	ScanIdentifier // identifier
	SkipIdentifier

	ScanKeyword // identifier
	SkipKeyword

	ScanType // identifier
	SkipType

	ScanOther // identifier
	SkipOther

	ScanOperator // operator
	SkipOperator

	ScanMath // operator
	SkipMath

	ScanSpace // space
	SkipSpace

	ScanQuote // string literal
	SkipQuote

	ScanRaw // string literal
	SkipRaw

	ScanRune // rune (character literal)
	SkipRune

	ScanBinary // number
	SkipBinary

	ScanOctal // number
	SkipOctal

	ScanDecimal // number
	SkipDecimal

	ScanHexadecimal // number
	SkipHexadecimal

	ScanFloating // number
	SkipFloating
)
const (
	ScanComment = ScanBlock | ScanLine
	SkipComment = SkipBlock | SkipLine

	ScanString = ScanQuote | ScanRaw
	SkipString = SkipQuote | SkipRaw

	ScanInteger = ScanBinary | ScanOctal | ScanDecimal | ScanHexadecimal
	SkipInteger = SkipBinary | SkipOctal | SkipDecimal | SkipHexadecimal

	ScanNumber = ScanInteger | ScanFloating // TBD: rational, complex, ...
	SkipNumber = SkipInteger | SkipFloating

	ScanGo = ScanCharacter | ScanComment | ScanIdentifier | ScanKeyword |
		ScanType | ScanOther | ScanOperator | ScanSpace | ScanString |
		ScanRune | ScanNumber
	ScanCalc = ScanGo | ScanMath
)

type tokenID struct {
	Type    Token
	Subtype Token
}

var predefinedMap = map[string]*tokenID{
	//reserved keywords
	"break":       {Keyword, 1},
	"case":        {Keyword, 2},
	"chan":        {Keyword, 3},
	"const":       {Keyword, 4},
	"continue":    {Keyword, 5},
	"default":     {Keyword, 6},
	"defer":       {Keyword, 7},
	"else":        {Keyword, 8},
	"fallthrough": {Keyword, 9},
	"for":         {Keyword, 10},
	"func":        {Keyword, 11},
	"go":          {Keyword, 12},
	"goto":        {Keyword, 13},
	"if":          {Keyword, 14},
	"import":      {Keyword, 15},
	"interface":   {Keyword, 16},
	"map":         {Keyword, 17},
	"package":     {Keyword, 18},
	"range":       {Keyword, 19},
	"return":      {Keyword, 20},
	"select":      {Keyword, 21},
	"struct":      {Keyword, 22},
	"switch":      {Keyword, 23},
	"try":         {Keyword, 24}, // new kid on the block
	"type":        {Keyword, 25},
	"var":         {Keyword, 26},

	// predeclared types
	"bool":       {Type, 1},
	"byte":       {Type, 2},
	"complex64":  {Type, 3},
	"complex128": {Type, 4},
	"float32":    {Type, 5},
	"float64":    {Type, 6},
	"int":        {Type, 7},
	"int8":       {Type, 8},
	"int16":      {Type, 9},
	"int32":      {Type, 10},
	"int64":      {Type, 11},
	"rune":       {Type, 12},
	"string":     {Type, 13},
	"uint":       {Type, 14},
	"uint8":      {Type, 15},
	"uint16":     {Type, 16},
	"uint32":     {Type, 17},
	"uint64":     {Type, 18},
	"uintptr":    {Type, 19},

	// other predeclared identifiers
	"append":  {Other, 1},
	"cap":     {Other, 2},
	"close":   {Other, 3},
	"complex": {Other, 4},
	"copy":    {Other, 5},
	"delete":  {Other, 6},
	"false":   {Other, 7},
	"imag":    {Other, 8},
	"iota":    {Other, 9},
	"len":     {Other, 10},
	"make":    {Other, 11},
	"new":     {Other, 12},
	"nil":     {Other, 13},
	"panic":   {Other, 14},
	"print":   {Other, 15},
	"println": {Other, 16},
	"real":    {Other, 17},
	"recover": {Other, 18},
	"true":    {Other, 19},
}

/*
var operatorMap = map[string]tokenType{
	// Go operators
	"--":  {lexOperator, lexOperatorGo},
	"-":   {lexOperator, lexOperatorGo},
	"-=":  {lexOperator, lexOperatorGo},
	",":   {lexOperator, lexOperatorGo},
	";":   {lexOperator, lexOperatorGo},
	":":   {lexOperator, lexOperatorGo},
	":=":  {lexOperator, lexOperatorGo},
	"!":   {lexOperator, lexOperatorGo},
	"!=":  {lexOperator, lexOperatorGo},
	"...": {lexOperator, lexOperatorGo},
	".":   {lexOperator, lexOperatorGo},
	"(":   {lexOperator, lexOperatorGo},
	")":   {lexOperator, lexOperatorGo},
	"[":   {lexOperator, lexOperatorGo},
	"]":   {lexOperator, lexOperatorGo},
	"{":   {lexOperator, lexOperatorGo},
	"}":   {lexOperator, lexOperatorGo},
	"*":   {lexOperator, lexOperatorGo},
	"*=":  {lexOperator, lexOperatorGo},
	"/":   {lexOperator, lexOperatorGo},
	"/=":  {lexOperator, lexOperatorGo},
	"&":   {lexOperator, lexOperatorGo},
	"&&":  {lexOperator, lexOperatorGo},
	"&^":  {lexOperator, lexOperatorGo},
	"&^=": {lexOperator, lexOperatorGo},
	"&=":  {lexOperator, lexOperatorGo},
	"%":   {lexOperator, lexOperatorGo},
	"%=":  {lexOperator, lexOperatorGo},
	"^":   {lexOperator, lexOperatorGo},
	"^=":  {lexOperator, lexOperatorGo},
	"+":   {lexOperator, lexOperatorGo},
	"++":  {lexOperator, lexOperatorGo},
	"+=":  {lexOperator, lexOperatorGo},
	"<-":  {lexOperator, lexOperatorGo},
	"<":   {lexOperator, lexOperatorGo},
	"<<":  {lexOperator, lexOperatorGo},
	"<<=": {lexOperator, lexOperatorGo},
	"<=":  {lexOperator, lexOperatorGo},
	"=":   {lexOperator, lexOperatorGo},
	"==":  {lexOperator, lexOperatorGo},
	">":   {lexOperator, lexOperatorGo},
	">=":  {lexOperator, lexOperatorGo},
	">>":  {lexOperator, lexOperatorGo},
	">>=": {lexOperator, lexOperatorGo},
	"|":   {lexOperator, lexOperatorGo},
	"|=":  {lexOperator, lexOperatorGo},
	"||":  {lexOperator, lexOperatorGo},

	// extended math operators (symbols and operations)
	"↑":   {lexOperator, lexOperatorMath},
	"÷":   {lexOperator, lexOperatorMath},
	"×":   {lexOperator, lexOperatorMath},
	"−":   {lexOperator, lexOperatorMath},
	"⊕":   {lexOperator, lexOperatorMath},
	"**":  {lexOperator, lexOperatorMath},
	"**=": {lexOperator, lexOperatorMath},
	"↑=":  {lexOperator, lexOperatorMath},
	"÷=":  {lexOperator, lexOperatorMath},
	"×=":  {lexOperator, lexOperatorMath},
	"−=":  {lexOperator, lexOperatorMath},
	"⊕=":  {lexOperator, lexOperatorMath},
}
*/

// Scan returns the next token, which is either a structured element or a single character.
// t is the token type, such as identifier or number, and v is its value as a string, such as
// "x" or "0153". Details (such as "ASCII-only" or "octal") are returned via the lex structure.
func (lex *Lexer) Scan() (Token, string) {
	if lex.Offset == 0 && lex.Bytes == 0 {
		lex.input = lex.Input // first call, set input to access original Input
	}

	for len(lex.Input) > 0 {
		// advance input position counters past prior token
		lex.Offset += lex.Bytes // advance through input bytes
		if nl := strings.Count(lex.Value, "\n"); nl == 0 {
			lex.Column += lex.Chars // no newline, so advance through this line
		} else { // one or more newlines, so reset column and advance in last line
			last := strings.LastIndexByte(lex.Value, '\n')            // just before last line
			lex.Column = utf8.RuneCountInString(lex.Value[last:]) - 1 // -1 for the prefix '\n'
			lex.Line += nl
		}
		// ...both offset and (line,column) now indicate the start of the next token.

		// Peek at the next character in input. If it decodes as an invalid Unicode character, the
		// value of c will be '\uFFFD'.
		c, size := utf8.DecodeRune(lex.Input)

		// fmt.Printf("offset=%5d, starts=%q\n", lex.Offset, lex.input[lex.Offset:lex.Offset+8])

		// Decode next token using this first character as anchor
		switch {
		// non-printing whitespace characters
		case lex.mode(ScanSpace) && (c == '\t' || c == '\n' || c == '\r' || c == ' '):
			lex.Chars, lex.Bytes = lex.match(func(c rune) bool { return c == '\t' || c == '\n' || c == '\r' || c == ' ' })
			lex.Value = lex.next(lex.Bytes)
			lex.Type, lex.Subtype = Space, 0
			if !lex.mode(SkipSpace) {
				return lex.Type, lex.Value
			}
		// line comment delimited by slash-slash and newline: "//" .* "\n"
		case lex.mode(ScanLine) && bytes.HasPrefix(lex.Input, []byte("//")):
			index := bytes.IndexByte(lex.Input, '\n')       // newline, if present, terminates...
			lex.Value = lex.next(If(index > -1, index, -1)) // ...but is not part of the comment.
			lex.Chars, lex.Bytes = utf8.RuneCountInString(lex.Value), len(lex.Value)
			lex.Type, lex.Subtype = Comment, Line
			if !lex.mode(SkipLine) {
				return lex.Type, lex.Value
			}
		// block comment delimited by slash-star and star-slash: "/*" .* "*/"
		case lex.mode(ScanBlock) && bytes.HasPrefix(lex.Input, []byte("/*")):
			index := bytes.Index(lex.Input, []byte("*/"))
			lex.Value = lex.next(If(index > -1, index+2, -1))
			lex.Chars, lex.Bytes = utf8.RuneCountInString(lex.Value), len(lex.Value)
			lex.Type, lex.Subtype = Comment, Block
			if !lex.mode(SkipBlock) {
				return lex.Type, lex.Value
			}
		// rune literal delimited by single quote: '...'
		case lex.mode(ScanRune) && c == '\'':
			escaped := false
			index := 0
			var ch byte
			for index, ch = range lex.Input {
				if index > 0 && ch == '\'' && !escaped {
					break
				} else if escaped {
					escaped = false
				} else if ch == '\\' {
					escaped = true
				}
				// else if c == '\n' {
				// break // bad news...illegal; bail here...is one choice
				// }
			}
			lex.Value = lex.next(If(index < len(lex.Input)-1, index+1, -1))
			lex.Chars, lex.Bytes = utf8.RuneCountInString(lex.Value), len(lex.Value)
			lex.Type, lex.Subtype = Rune, 0
			if !lex.mode(SkipRune) {
				return lex.Type, lex.Value
			}

		// character string delimited by double quote: " ... "
		case lex.mode(ScanQuote) && c == '"':
			escaped := false
			index := 0
			var ch byte
			for index, ch = range lex.Input {
				if index > 0 && ch == '"' && !escaped {
					break
				} else if escaped {
					escaped = false
				} else if c == '\\' {
					escaped = true
				}
				// else if c == '\n' {
				// 	break // bad news...illegal; bail here...is one choice
				// }
			}
			lex.Value = lex.next(If(index < len(lex.Input)-1, index+1, -1))
			lex.Chars, lex.Bytes = utf8.RuneCountInString(lex.Value), len(lex.Value)
			lex.Type, lex.Subtype = String, Quote
			if !lex.mode(SkipQuote) {
				return lex.Type, lex.Value
			}
		// single character operators "{}[]():;,." are the most frequent in go source code
		case lex.mode(ScanOperator) && strings.ContainsRune("{}[]():;,.", c):
			lex.Chars, lex.Bytes = 1, 1

			// is this a real number?
			real := false
			if lex.Input[0] == '.' && len(lex.Input) > 1 {
				// extract rest of number (if any)
				ch := lex.Input[lex.Bytes]
				if ch < '0' || ch > '9' {
					goto doneA
				}
				for '0' <= ch && ch <= '9' { // move the digit to the token
					lex.Chars++
					lex.Bytes++
					real = true
					if len(lex.Input) > lex.Bytes {
						ch = lex.Input[lex.Bytes]
					} else {
						break
					}
				}
				if ch == 'e' || ch == 'E' { // move the 'e' to the token
					lex.Chars++
					lex.Bytes++
					real = true
					if len(lex.Input) > lex.Bytes {
						ch = lex.Input[lex.Bytes]
					} else {
						goto doneA
					}
					if ch == '+' || ch == '-' { // move the '+/-' to the token
						lex.Chars++
						lex.Bytes++
						real = true
						if len(lex.Input) > lex.Bytes {
							ch = lex.Input[lex.Bytes]
						} else {
							// actually, a parse error
							goto doneA
						}
					}
					for '0' <= ch && ch <= '9' { // move the digit to the token
						lex.Chars++
						lex.Bytes++
						real = true
						if len(lex.Input) > lex.Bytes {
							ch = lex.Input[lex.Bytes]
						} else {
							break
						}
					}
				}
			doneA:
			}
			// yes it is. do we want to have parsed it?
			if real {
				lex.Value = lex.next(lex.Bytes)
				lex.Type, lex.Subtype = Number, Floating
				if !lex.mode(SkipFloating) {
					return lex.Type, lex.Value
				}
				continue
			}

			// no, it is a single-character operator (almost always)
			lex.Chars, lex.Bytes = 1, 1 // matched characters are all single-byte
			lex.Value = lex.next(lex.Bytes)
			lex.Type, lex.Subtype = Operator, OperatorGo
			if !lex.mode(SkipOperator) {
				return lex.Type, lex.Value
			}

		// identifier: letter/underscore then letter/underscore/digit; letters optionally ASCII-only
		case lex.mode(ScanIdentifier|ScanKeyword|ScanType) && (('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || (c == '_') || (!lex.ASCII && unicode.IsLetter(c))):
			lex.Chars, lex.Bytes = lex.match(func(c rune) bool {
				return ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || (c == '_') || ('0' <= c && c <= '9') || (!lex.ASCII && unicode.IsLetter(c))
			})
			// is it a predefined identifier? (historical note: the initial role of hash tables)
			if id, ok := predefinedMap[string(lex.Input[:lex.Bytes])]; ok {
				lex.Value = lex.next(lex.Bytes)
				lex.Type, lex.Subtype = id.Type, id.Subtype
				// if (lex.Type == Keyword && !lex.mode(SkipKeyword)) ||
				// 	(lex.Type == Type && !lex.mode(SkipType)) ||
				// 	(lex.Type == Other && !lex.mode(SkipOther)) {
				// 	return lex.Type, lex.Value
				// }
				if !lex.mode(skipToken[-lex.Type]) {
					return lex.Type, lex.Value
				}
				continue
			}
			// ...no, so it is a user  identifier...
			if lex.mode(ScanIdentifier) {
				lex.Value = lex.next(lex.Bytes)
				lex.Type, lex.Subtype = Identifier, Token(If(lex.Chars == lex.Bytes, ASCII, Unicode))
				if !lex.mode(SkipIdentifier) {
					return lex.Type, lex.Value
				}
				continue
			}
			// ...but we're not handling those so  treat it as a single-character.
			lex.Value = lex.next(size) // size is byte width of scanned UTF-8 rune
			lex.Chars, lex.Bytes = 1, size
			// lex.Type, lex.Subtype = lexCharacter, lexToken(c) // preferred API, but to match scanner...
			lex.Type, lex.Subtype = Token(c), 0 // use character as token code (rather than lexCharacter)
			if !lex.mode(SkipCharacter) {
				return lex.Type, lex.Value
			}
		// multi-character operators: "+", "<<", "&^=", ...
		case lex.mode(ScanOperator) && strings.ContainsRune("+-*/^%<>&|!=", c):
			match := 0
			for _, op := range [][]byte{
				[]byte("||"),
				[]byte("|="),
				[]byte(">>="),
				[]byte(">>"),
				[]byte(">="),
				[]byte("=="),
				[]byte("<="),
				[]byte("<<="),
				[]byte("<<"),
				[]byte("<-"),
				[]byte("+="),
				[]byte("++"),
				[]byte("^="),
				[]byte("%="),
				[]byte("&="),
				[]byte("&^="),
				[]byte("&^"),
				[]byte("&&"),
				[]byte("/="),
				[]byte("*="),
				[]byte("..."),
				[]byte("!="),
				[]byte(":="),
				[]byte("-="),
				[]byte("--")} {
				if bytes.HasPrefix(lex.Input, op) {
					match = len(op)
					break
				}
			}
			lex.Value = lex.next(If(match != 0, match, size))
			lex.Chars, lex.Bytes = len(lex.Value), len(lex.Value) // matched characters are all single-byte
			lex.Type, lex.Subtype = Operator, OperatorGo
			if !lex.mode(SkipOperator) {
				return lex.Type, lex.Value
			}
		// raw character string delimited by unapostrophe: "`" ... "`"
		case lex.mode(ScanRaw) && c == '`':
			// "Alternatively referred to as an acute, backtick, grave, grave accent, left quote,
			// open quote, or a push, the back quote or backquote is a punctuation mark (`). It is
			// on the same U.S. computer keyboard key as the tilde. Although not as common as the
			// above uses, the back quote is also sometimes referred to as a back prime, back tick,
			// birk, blugle, quasiquote, and unapostrophe."
			// https://www.computerhope.com/jargon/b/backquot.htm
			prefix := lex.next(1)                    // the leading unapostrophe
			index := bytes.IndexByte(lex.Input, '`') // the trailing unapostrophe
			lex.Value = prefix + lex.next(If(index > -1, index+1, -1))
			lex.Chars, lex.Bytes = utf8.RuneCountInString(lex.Value), len(lex.Value)
			lex.Type, lex.Subtype = String, Raw
			if !lex.mode(SkipRaw) {
				return lex.Type, lex.Value
			}
		// prefixed binary: "0b1", "0B101"
		case lex.mode(ScanBinary) && c == '0' && len(lex.Input) > 1 && (lex.Input[1] == 'b' || lex.Input[1] == 'B'):
			prefix := lex.next(2) // to preserve prefix case
			_, bytes := lex.match(func(c rune) bool { return ('0' <= c && c <= '1') || c == '_' })
			lex.Value = prefix + lex.next(bytes)
			lex.Chars, lex.Bytes = len(lex.Value), len(lex.Value) // length with prefix
			lex.Type, lex.Subtype = Number, Binary
			if !lex.mode(SkipBinary) {
				return lex.Type, lex.Value
			}
		// prefixed octal: "0o17", "0O237",
		case lex.mode(ScanOctal) && c == '0' && len(lex.Input) > 1 && (lex.Input[1] == 'o' || lex.Input[1] == 'O'):
			prefix := lex.next(2) // to preserve prefix case
			_, bytes := lex.match(func(c rune) bool { return ('0' <= c && c <= '7') || c == '_' })
			lex.Value = prefix + lex.next(bytes)
			lex.Chars, lex.Bytes = len(lex.Value), len(lex.Value) // length with prefix
			lex.Type, lex.Subtype = Number, Octal
			if !lex.mode(SkipOctal) {
				return lex.Type, lex.Value
			}
		// prefixed hexadecimal: "0x399a", "0X400A"
		case lex.mode(ScanHexadecimal) && c == '0' && len(lex.Input) > 1 && (lex.Input[1] == 'x' || lex.Input[1] == 'X'):
			prefix := lex.next(2) // to preserve prefix case
			_, bytes := lex.match(func(c rune) bool {
				return ('0' <= c && c <= '9') || ('a' <= c && c <= 'f') || ('A' <= c && c <= 'F') || c == '_'
			})
			lex.Value = prefix + lex.next(bytes)
			lex.Chars, lex.Bytes = len(lex.Value), len(lex.Value) // length with prefix
			lex.Type, lex.Subtype = Number, Hexadecimal
			if !lex.mode(SkipHexadecimal) {
				return lex.Type, lex.Value
			}
		// decimal or legacy-form octal with leading zero "280", "0262"
		case lex.mode(ScanDecimal|ScanOctal|ScanFloating) && ('0' <= c && c <= '9'):
			lex.Chars, lex.Bytes = lex.match(func(c rune) bool { return ('0' <= c && c <= '9') || c == '_' })
			// note: matched characters>1 restriction is to treat "0" as decimal zero and "00" as octal zero.
			if lex.Chars > 1 && lex.Input[0] == '0' && !bytes.ContainsAny(lex.Input[:lex.Bytes], "89") {
				// matched token is legacy octal. do we want to have parsed it?
				if lex.mode(ScanOctal) { // yes...
					lex.Value = lex.next(lex.Bytes)
					lex.Type, lex.Subtype = Number, Octal // form is legacy octal
					if !lex.mode(SkipOctal) {
						return lex.Type, lex.Value
					}
					continue
				}
			} else {
				//is this a real number?
				real := false
				if len(lex.Input) > lex.Bytes {
					ch := lex.Input[lex.Bytes]
					// extract rest of number
					if ch == '.' { // move the decimal point to the token
						lex.Chars++
						lex.Bytes++
						if len(lex.Input) > lex.Bytes {
							ch = lex.Input[lex.Bytes]
						}
						for '0' <= ch && ch <= '9' { // move the digit to the token
							lex.Chars++
							lex.Bytes++
							real = true
							if len(lex.Input) > lex.Bytes {
								ch = lex.Input[lex.Bytes]
							} else {
								break
							}
						}
					}
					if ch == 'e' || ch == 'E' { // move the 'e' to the token
						lex.Chars++
						lex.Bytes++
						real = true
						if len(lex.Input) > lex.Bytes {
							ch = lex.Input[lex.Bytes]
						} else {
							goto done
						}
						if ch == '+' || ch == '-' { // move the '+/-' to the token
							lex.Chars++
							lex.Bytes++
							real = true
							if len(lex.Input) > lex.Bytes {
								ch = lex.Input[lex.Bytes]
							} else {
								// actually, a parse error
								goto done
							}
						}
						for '0' <= ch && ch <= '9' { // move the digit to the token
							lex.Chars++
							lex.Bytes++
							real = true
							if len(lex.Input) > lex.Bytes {
								ch = lex.Input[lex.Bytes]
							} else {
								break
							}
						}
					}
				done:
				}
				// matched token is floating point. do we want to have parsed it?
				if real {
					lex.Value = lex.next(lex.Bytes)
					lex.Type, lex.Subtype = Number, Floating
					if !lex.mode(SkipFloating) {
						return lex.Type, lex.Value
					}
					continue
				}
				// matched token is decimal. do we want to have parsed it?
				if lex.mode(ScanDecimal) { // yes...
					lex.Value = lex.next(lex.Bytes)
					lex.Type, lex.Subtype = Number, Decimal
					if !lex.mode(SkipDecimal) {
						return lex.Type, lex.Value
					}
					continue
				}
			}
			// ...no, we did not want to parse it. it's octal when we don't want octal, or likewise
			// with decimal and real; so disregard the match and handle as a single-character
			// case.
			lex.Value = lex.next(size) // size is byte width of scanned UTF-8 rune
			lex.Chars, lex.Bytes = 1, size
			// lex.Type, lex.Subtype = lexCharacter, lexToken(c) // preferred API, but to match scanner...
			lex.Type, lex.Subtype = Token(c), 0 // use character as token code (rather than lexCharacter)
			if !lex.mode(SkipCharacter) {
				return lex.Type, lex.Value
			}
		// extra operators: math-symbol synonyms ("−×÷⊕") and exponentiation ("**" and "↑")
		case lex.mode(ScanMath) && strings.ContainsRune("−×÷⊕↑", c):
			chars := 2
			match := 0
			for index, op := range [][]byte{
				[]byte("**="),
				[]byte("**"),
				[]byte("−="),
				[]byte("×="),
				[]byte("÷="),
				[]byte("⊕="),
				[]byte("↑=")} {
				if bytes.HasPrefix(lex.Input, op) {
					match = len(op)
					if index == 0 {
						chars = 3
					}
					break
				}
			}
			if match != 0 {
				lex.Value = lex.next(match) // multicharacter operator detected by HasPrefix() lookahead
				lex.Chars, lex.Bytes = chars, len(lex.Value)
			} else {
				lex.Value = lex.next(size) // single character operator matched by ContainsRune()
				lex.Chars, lex.Bytes = 1, len(lex.Value)
			}
			lex.Type, lex.Subtype = Operator, OperatorMath
			if !lex.mode(SkipMath) {
				return lex.Type, lex.Value
			}
		// no prefix match so lexeme is this character without interpretation: "?", "⌘"
		default:
			lex.Value = lex.next(size) // size is byte width of scanned UTF-8 rune
			lex.Chars, lex.Bytes = 1, size
			// lex.Type, lex.Subtype = lexCharacter, lexToken(c) // preferred API, but to match scanner...
			lex.Type, lex.Subtype = Token(c), 0 // use character as token code (rather than lexCharacter)
			if !lex.mode(SkipCharacter) {
				return lex.Type, lex.Value
			}
		}
	}
	return EOF, ""
}

// Match characters in the input based on matchFunc(). Returns the number of characters (code
// points) matched and the number of bytes they contain. When chars==bytes, input is ASCII.
func (lex *Lexer) match(matchFunc func(rune) bool) (chars, bytes int) {
	c, size := utf8.DecodeRune(lex.Input[bytes:])
	for bytes < len(lex.Input) && matchFunc(c) {
		chars += 1
		bytes += size
		c, size = utf8.DecodeRune(lex.Input[bytes:])
	}
	return
}

// Move bytes from the lex input source and return them to the caller. When requested byte count is
// less than one, the remainder of the input stream is moved and future reads will result in EOF.
func (lex *Lexer) next(bytes int) (s string) {
	switch {
	case bytes < 0:
		s, lex.Input = string(lex.Input), nil
		// fmt.Printf("EAT count=%d text=%q\n", len(s), s)
		// fmt.Printf("!EAT count=%d line=%d trim=%q\n", len(s), lex.Line, s[:32])
	default:
		s, lex.Input = string(lex.Input[:bytes]), lex.Input[bytes:]
	}
	return
}

// Test if any bit in mask is set in the lex Mode; this asks "are any of these are enabled?"
func (lex *Lexer) mode(mask uint) bool {
	return lex.Mode&mask != 0
}

// Boolean selector returning a or b as test is true or false, respectively. Elegant simplifier in
// assignment statements as it makes the target variable and intent to assign clear and plain.
// Commonly reduces five lines of sparse source code to the letters "If". Each argument is
// evaluated before the call, so not the same as the C-language ternary operator. Inlines.
func If(test bool, a, b int) int {
	if test {
		return a
	}
	return b
}

// Extract line containing current position
func (lex *Lexer) GetLine() string {
	here := lex.Offset
	// fmt.Printf("here=%d, lex.input[here..]=%s\n", here, lex.input[here:here+8])
	if lex.input[here] == '\n' {
		return "\n"
	}
	//look back
	prior := bytes.LastIndex(lex.input[:here], []byte("\n"))
	if prior == -1 {
		prior = 0 // start of input
	} else {
		prior++ // line starts at charcter after prior newline
	}

	// look forward
	next := bytes.Index(lex.input[here:], []byte("\n"))
	if next == -1 {
		next = len(lex.input) // line ends with input and is unterminated
	} else {
		next += here // we scanned only the tail of the string
	}

	// line is range from just after last newline to just before next
	return string(lex.input[prior:next])
}
