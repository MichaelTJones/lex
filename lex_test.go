package lex

import (
	"testing"
)

func TestSanity(t *testing.T) {
	tests := []struct {
		mode  uint
		input string
	}{
		{ScanCalc, "hello world"},
		{ScanCalc | SkipSpace, "hello world"},
		{ScanCalc | SkipSpace, "a abc abc123 abc_def"},

		{ScanCalc | SkipSpace, `"hello world"`},
		{ScanCalc | SkipSpace, `"hello wo`}, // unterminated due to early EOF
		{ScanCalc | SkipSpace, "`hello wo"}, // unterminated due to early EOF

		{ScanCalc | SkipSpace, "/* hello world */"},
		{ScanCalc | SkipSpace, "//hello world\n"},
		{ScanCalc | SkipSpace, "/* hello wo"}, // unterminated due to early EOF
		{ScanCalc | SkipSpace, "//hello wo"},  // unterminated due to early EOF

		{ScanCalc | SkipSpace, "+ - * / ! % ^ & | &^ < > << >> += -= || &&"},
		{ScanCalc | SkipSpace, "−×÷⊕↑ −= ×= ÷= ⊕= ↑="},

		{ScanCalc | SkipSpace, "0b101 0B111 0o377 0O666 00 0 123 123_456 0xacceded 0Xfabaceae"},
		{ScanCalc | SkipSpace, "0888"}, // semi-valid lexically, invalid semantically: decimal with octal prefix

		{ScanCalc, "a += 5"},
		{ScanCalc, "ಓ += 5"}, // Kannada letter 'Go' (https://en.wikipedia.org/wiki/Kannada)

		{ScanCalc, "1 2. 3.3 .4 5e5 6.e6 7.7e7 .8e1 9e+9 10e-10"},

		{ScanCalc | SkipSpace, "~!@#$"},
		{ScanCalc | SkipSpace, "© ໙ ⾕ 㑇 /* clever */ ελπίδα /* hope */"},

		{ScanCalc | SkipSpace | SkipComment, `
// This is scanned code.
if a > 10 {
	someParsable = text
}`},

		{ScanCalc | ((ScanCalc << 1) &^ SkipComment), `
    // Comment begins at column 5.

This line should not be included in the output.

/*
This multiline comment
should be extracted in
its entirety.
*/
`},
	}

	for i, test := range tests {
		// lex := &Lex{Input: test.input, Mode: test.mode}
		lex := &Lexer{Input: []byte(test.input), Mode: test.mode, Position: Position{Filename: "ψ"}}
		collected := ""

		t.Logf("test #%v\n", i)
		t.Logf("  input = %q\n", test.input)

		for tok, text := lex.Scan(); tok != EOF; tok, text = lex.Scan() {
			collected += text
			t.Logf("  token = %-16v subtype = %1d  position = %-10v  text = %q\n", tok, lex.Subtype, lex.Position, text)
		}

		if collected == test.input {
			t.Logf("collected results match the original\n")
		}
	}
}
