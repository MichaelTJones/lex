// Code generated by "stringer -type=Token"; DO NOT EDIT.

package lex

import "strconv"

func _() {
	// An "invalid array index" compiler error signifies that the constant values have changed.
	// Re-run the stringer command to generate them again.
	var x [1]struct{}
	_ = x[Character - -1]
	_ = x[Comment - -2]
	_ = x[Identifier - -3]
	_ = x[Keyword - -4]
	_ = x[Operator - -5]
	_ = x[Rune - -6]
	_ = x[Space - -7]
	_ = x[String - -8]
	_ = x[Type - -9]
	_ = x[Other - -10]
	_ = x[Number - -11]
	_ = x[EOF - -12]
}

const _Token_name = "EOFNumberOtherTypeStringSpaceRuneOperatorKeywordIdentifierCommentCharacter"

var _Token_index = [...]uint8{0, 3, 9, 14, 18, 24, 29, 33, 41, 48, 58, 65, 74}

func (i Token) String() string {
	i -= -12
	if i < 0 || i >= Token(len(_Token_index)-1) {
		return "Token(" + strconv.FormatInt(int64(i+-12), 10) + ")"
	}
	return _Token_name[_Token_index[i]:_Token_index[i+1]]
}
