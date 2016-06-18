package kek

enum class TokenType(val keyword: String = "", val hasOverlap: Boolean = false) {

    L_PARA("("),
    R_PARA(")"),
    L_BRACK("["),
    R_BRACK("]"),
    DOT("."),
    COLON(":"),
    COMMA(","),
    PLUS("+"),
    MINUS("-"),
    MUL("*"),
    DIV("/"),
    GREATEREQUAL(">="),
    GREATER(">"),
    LESSEQUAL("<="),
    LESS("<"),
    DOUBLE_EQUAL("=="),
    EQUAL("="),
    EXCLAMATION("!"),

    IF("if", true),
    THEN("then", true),
    ELSE("else", true),
    WHILE("while", true),
    DO("do", true),
    FOR("for", true),
    VAL("val", true),
    VAR("var", true),
    AND("and", true),
    OR("or", true),
    NOT("not", true),
    XOR("xor", true),
    TRUE("true", true),
    FALSE("false", true),

    NUMBER(),
    CHARACTER(),
    STRING(),
    IDENTIFIER(),

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

        // match keywords
        for (type in TokenType.values()) {
            if (type.keyword.isNotEmpty() and !type.hasOverlap) {
                if (tryMatch(state, type.keyword))
                    state.tokens.add(Token(type, state.index - type.keyword.length, state.line, state.lineStart - type.keyword.length, type.keyword))
            }
        }

        // match literals
        tryMatchNumber(state)
        tryMatchString(state)
        tryMatchCharacter(state)

        // match identifiers or overlapping keywords
        tryMatchIdentifierOrKeyword(state)

        if (lastIndex == state.index) {
            throw CompilerException(state.input.toString(), "Unknown token", state.line, state.lineStart, state.lineStart)
        }
        lastIndex = state.index
    }
}

private fun tryMatchCharacter(state: LexerState) {
    if (isEOF(state)) return
    var c = state.input[state.index]
    if (c != '\'') return

    val start = state.index
    val lineStart = state.lineStart

    nextChar(state)
    if (isEOF(state)) throw CompilerException(state.input.toString(), "Expected character to be closed via \'", state.line, lineStart, state.lineStart)

    c = state.input[state.index]
    if (c == '\\') {
        nextChar(state)
        if (isEOF(state)) throw CompilerException(state.input.toString(), "Expected escape sequence to be completed with \\, \', n, t", state.line, lineStart, state.lineStart)

        c = state.input[state.index]
        if ((c != '\\') and (c != '\'') and (c != 'n') and (c != 't')) {
            throw CompilerException(state.input.toString(), "Expected escape sequence to be completed with \\, \', n, t", state.line, lineStart, state.lineStart)
        }
        nextChar(state)
    } else if (c == '\'') {
        nextChar(state)
        state.tokens.add(Token(TokenType.CHARACTER, start, state.line, lineStart, state.input.substring(start, state.index)))
        return
    } else {
        nextChar(state)
    }

    if (isEOF(state)) throw CompilerException(state.input.toString(), "Expected character to be closed via \'", state.line, lineStart, state.lineStart)
    if ((state.input[state.index] != '\'')) throw CompilerException(state.input.toString(), "Expected character to be closed via \'", state.line, lineStart, state.lineStart)
    nextChar(state)

    state.tokens.add(Token(TokenType.CHARACTER, start, state.line, lineStart, state.input.substring(start, state.index)))
}

private fun tryMatchIdentifierOrKeyword(state: LexerState) {
    if (isEOF(state)) return
    var c = state.input[state.index]
    if (!Character.isLetter(c)) return

    val start = state.index
    val lineStart = state.lineStart
    while (Character.isLetter(c) or Character.isDigit(c) or (c == '_')) {
        nextChar(state)
        if (isEOF(state)) {
            break;
        }
        c = state.input[state.index]
    }

    // identifiers may overlap with keywords, need to fix up
    // token type
    val tokenText = state.input.substring(start, state.index)
    var tokenType = TokenType.IDENTIFIER
    for (type in TokenType.values()) {
        if (type.keyword.isNotEmpty() and type.hasOverlap) {
            if (tokenText == type.keyword) {
                tokenType = type
            }
        }
    }
    state.tokens.add(Token(tokenType, start, state.line, lineStart, state.input.substring(start, state.index)))
}

private fun tryMatchString(state: LexerState) {
    if (isEOF(state)) return
    var c = state.input[state.index]
    if (c != '"') return

    val start = state.index
    val lineStart = state.lineStart

    nextChar(state)
    do {
        if (isEOF(state)) throw CompilerException(state.input.toString(), "Expected string to be closed via \"", state.line, lineStart, state.lineStart)

        c = state.input[state.index]
        if (c == '\\') {
            nextChar(state)
            if (isEOF(state)) throw CompilerException(state.input.toString(), "Expected escape sequence to be completed with \\, \", n, t", state.line, lineStart, state.lineStart)

            c = state.input[state.index]
            if ((c != '\\') and (c != '\"') and (c != 'n') and (c != 't')) {
                throw CompilerException(state.input.toString(), "Expected escape sequence to be completed with \\, \", n, t", state.line, lineStart, state.lineStart)
            }
            nextChar(state)
        } else if (c == '"') {
            nextChar(state)
            state.tokens.add(Token(TokenType.STRING, start, state.line, lineStart, state.input.substring(start, state.index)))
            return
        } else {
            nextChar(state)
        }
    } while (true)
}

private fun tryMatchNumber(state: LexerState) {
    if (isEOF(state)) return
    var c = state.input[state.index]
    if (!Character.isDigit(c)) return

    val start = state.index
    val lineStart = state.lineStart
    while (Character.isDigit(c)) {
        nextChar(state)
        if (isEOF(state)) {
            state.tokens.add(Token(TokenType.NUMBER, start, state.line, lineStart, state.input.substring(start)))
            return
        }
        c = state.input[state.index]
    }

    if (c != '.') {
        state.tokens.add(Token(TokenType.NUMBER, start, state.line, lineStart, state.input.substring(start, state.index)))
        return
    }

    nextChar(state)
    if (isEOF(state)) {
        state.tokens.add(Token(TokenType.NUMBER, start, state.line, lineStart, state.input.substring(start, state.index)))
        return
    }

    c = state.input[state.index]
    while (Character.isDigit(c)) {
        nextChar(state)
        if (isEOF(state)) {
            state.tokens.add(Token(TokenType.NUMBER, start, state.line, lineStart, state.input.substring(start)))
            return
        }
        c = state.input[state.index]
    }

    state.tokens.add(Token(TokenType.NUMBER, start, state.line, lineStart, state.input.substring(start, state.index)))
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
            while (true) {
                if (isEOF(state)) throw CompilerException(input.toString(), "Line or block comment wasn't completed", state.line, state.lineStart, state.lineStart)
                if (!skipNewLine(state)) {
                    if (tryMatch(state, "*/")) return
                    nextChar(state)
                }
            }
        }
    }
}