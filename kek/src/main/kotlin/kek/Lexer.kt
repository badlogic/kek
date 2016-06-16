package kek

enum class TokenType(val keyword: String = "") {

    // special characters
    L_PARA("("),
    R_PARA(")"),
    L_BRACK("["),
    R_BRACK("]"),
    COLON(":"),
    COMMA(","),
    PLUS("+"),
    MINUS("-"),
    MUL("*"),
    DIV("/"),

    // keywords
    IF("if"),
    THEN("then"),
    ELSE("else"),
    DEF("def"),
    VAL("val"),
    VAR("var"),

    // literals
    IDENTIFIER(),
    NUMBER(),
    EOF(),
}

data class Token(val type: TokenType,
                 val index: Int,
                 val line: Int,
                 val lineStart: Int,
                 val text: String)

private data class LexerState(var input: CharSequence = "",
                              var index: Int = 0,
                              var line: Int = 1,
                              var lineStart: Int = 1,
                              var tokens: MutableList<Token> = mutableListOf<Token>())

fun tokenize(input: CharSequence): List<Token> {
    val state = LexerState(input)
    var lastIndex = 0
    while (true) {
        // Check EOF
        if (isEOF(state)) {
            with (state) {
                tokens.add(Token(TokenType.EOF, index, line, lineStart, ""))
                return tokens
            }
        }

        // skip whitespace & comments
        skipWhitespace(state)
        skipComments(state)

        // match special chars
        for (type in TokenType.values()) {
            if (type.keyword.isNotEmpty()) {
                if (tryMatch(state, type.keyword))
                    state.tokens.add(Token(type, state.index - type.keyword.length, state.line, state.lineStart - type.keyword.length, type.keyword))
            }
        }

        if (lastIndex == state.index) {
            throw CompilerException(state.input.toString(), "Unknown token", state.line, state.lineStart, state.lineStart)
        }
        lastIndex = state.index;
    }
}

private fun tryMatch(state: LexerState, needle: String): Boolean {
    with(state) {
        if (input.length <= index + needle.length - 1) return false
        if (input.startsWith(needle, index)) {
            index += needle.length
            lineStart += needle.length
            return true
        } else {
            return false
        }
    }
}

private fun nextChar(state: LexerState) {
    with(state) {
        index++
        lineStart++
    }
}

private fun isEOF(state: LexerState, lookAhead: Int = 0): Boolean {
    return state.index + lookAhead >= state.input.length
}

private fun skipNewLine(state: LexerState): Boolean {
    if (isEOF(state)) return false

    with(state) {
        if (input[index] == '\n') {
            index++
            line++
            lineStart = 1
            return true
        } else {
            return false
        }
    }
}

private fun skipWhitespace(state: LexerState) {
    while (!isEOF(state)) {
        with(state) {
            val c = input[index]
            if (!Character.isWhitespace(c)) return
            if (!skipNewLine(state)) nextChar(state)
        }
    }
}

private fun skipComments(state: LexerState) {
    if (isEOF(state)) return

    with(state) {
        var c = input[index]

        if (tryMatch(state, "//")) {
            while (true) {
                if (isEOF(state)) return
                if (skipNewLine(state)) return
                nextChar(state)
            }
        }

        if (tryMatch(state, "/*")) {
            while(true) {
                if (isEOF(state)) throw CompilerException(input.toString(), "Line or block comment wasn't completed", state.line, state.lineStart, state.lineStart)
                if (!skipNewLine(state)) {
                    if (tryMatch(state, "*/")) return
                    nextChar(state)
                }
            }
        }
    }
}