package kek

interface AstNode {
}

class CompilationUnit : AstNode {
}

class FuncionDefinition : AstNode {
}

class ParameterList : AstNode {
}

class Parameter : AstNode {
}

open class Type : AstNode {
}

class NoType : Type() {
}

class FunctionBody : AstNode {
}

private data class ParserState(val input: CharSequence = "",
                               val tokens: List<Token>,
                               var currentToken: Int = 0,
                               val nodeStack: MutableList<AstNode> = mutableListOf<AstNode>())

fun parse(input: CharSequence): AstNode {
    val tokens = tokenize(input)
    val state = ParserState(input, tokens)
    return compilationUnit(state)
}

private fun match(state: ParserState, tokenType: TokenType, advanceIfMatched: Boolean = false): Boolean {
    if (state.tokens.size == state.currentToken) throw RuntimeException("EOF")
    val result = state.tokens[state.currentToken].type == tokenType
    if (result && advanceIfMatched) next(state)
    return result
}

private fun next(state: ParserState): Token {
    if (state.tokens.size == state.currentToken) throw RuntimeException("EOF")
    return state.tokens[state.currentToken++]
}

private fun error(state: ParserState, msg: String) {
    val token = state.tokens[state.currentToken]
    throw CompilerException(state.input.toString(), msg + ", got " + token.text,
            token.line, token.column, token.column + token.text.length)
}

private fun compilationUnit(state: ParserState): CompilationUnit {
    val functions = mutableListOf<AstNode>()
    val structs = mutableListOf<AstNode>()

    while (!match(state, TokenType.EOF)) {
        if (match(state, TokenType.FUNCTION)) {
            functions.add(function(state))
        } else if (match(state, TokenType.STRUCTURE)) {

        } else {
            error(state, "Expected a function or structure definition")
        }
    }

    // FIXME
    return CompilationUnit()
}

private fun function(state: ParserState): FuncionDefinition {
    if (!match(state, TokenType.FUNCTION, true)) error(state, "Expected a function definition")
    if (!match(state, TokenType.IDENTIFIER)) error(state, "Expected function name")
    val name: Token = next(state)
    val parameterList = parameterList(state)

    var returnType: Type = NoType();
    if (match(state, TokenType.COLON, true)) {
        returnType = type(state);
    }

    val body = functionBody(state)

    if (!match(state, TokenType.END, true)) error(state, "Expected 'end' at end of function definition")

    // FIXME
    return FuncionDefinition()
}

private fun functionBody(state: ParserState): FunctionBody {
    // FIXME
    return FunctionBody();
}

private fun parameterList(state: ParserState): ParameterList {
    if (!match(state, TokenType.L_PARA, true)) error(state, "Expected a parameter list, starting with (")
    while (!match(state, TokenType.R_PARA)) {
        val param = parameter(state)
        if (!match(state, TokenType.R_PARA)) {
            if (!match(state, TokenType.COMMA, true)) error(state, "Expected a coma (,) between parameters");
        }
    }
    if (!match(state, TokenType.R_PARA, true)) error(state, "Expected the parameter list to end with )")

    // FIXME
    return ParameterList()
}

private fun parameter(state: ParserState): Parameter {
    if (!match(state, TokenType.IDENTIFIER)) error(state, "Expected a parameter name")
    val name = next(state)
    if (!match(state, TokenType.COLON, true)) error(state, "Expected a colon (:) between the parameter name and parameter type")
    val type = type(state)

    // FIXME
    return Parameter()
}

private fun type(state: ParserState): Type {
    if (!match(state, TokenType.IDENTIFIER)) error(state, "Expected a type")
    val typeIdentifier = next(state)

    // FIXME
    return Type()
}