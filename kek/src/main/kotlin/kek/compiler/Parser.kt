package kek.compiler

import kek.runtime.*

private data class ParserState(val source: Source,
                               val tokens: List<Token>,
                               var currentToken: Int = 0,
                               val nodeStack: MutableList<AstNode> = mutableListOf<AstNode>()) {

    fun current(): Token {
        return tokens[currentToken]
    }

    fun last(): Token {
        return tokens[currentToken - 1]
    }
}

fun parse(input: Source): CompilationUnitNode {
    val tokens = tokenize(input)
    val state = ParserState(input, tokens)
    return compilationUnit(state)
}

private fun match(state: ParserState, tokenTypes: List<TokenType>): Boolean {
    for (type in tokenTypes) {
        if (match(state, type)) return true
    }
    return false
}

private fun match(state: ParserState, tokenType: TokenType, advanceIfMatched: Boolean = false): Boolean {
    if (state.tokens.size == state.currentToken) throw RuntimeException("Reached end of file, expected some more code")
    val result = state.tokens[state.currentToken].type == tokenType
    if (result && advanceIfMatched) next(state)
    return result
}

private fun next(state: ParserState): Token {
    if (state.tokens.size == state.currentToken) throw RuntimeException("Reached end of file, expected some more code")
    return state.tokens[state.currentToken++]
}

private fun error(state: ParserState, msg: String) {
    val token = state.tokens[state.currentToken]
    throw CompilerException(state.source, msg + ", got " + token.text,
            token.line, token.column, token.column + token.text.length)
}

private fun compilationUnit(state: ParserState): CompilationUnitNode {
    val module = module(state)
    val imports = mutableListOf<ImportNode>()
    val functions = mutableListOf<FunctionNode>()
    val structs = mutableListOf<StructureNode>()

    while (!match(state, TokenType.EOF)) {
        if (match(state, TokenType.FUNCTION) || match(state, TokenType.EXTERN)) {
            functions.add(function(state))
        } else if (match(state, TokenType.STRUCTURE)) {
            structs.add(structure(state))
        } else if (match(state, TokenType.IMPORT)) {
            imports.add(import(state))
        } else {
            error(state, "Expected an import, a function or structure definition")
        }
    }

    val eof = Token(TokenType.EOF, 0, 0, 0, "")
    val cu = if (state.tokens.size > 0)
        CompilationUnitNode(state.source, module, imports, functions, structs, state.tokens.first(), state.tokens.last())
    else
        CompilationUnitNode(state.source, module, imports, functions, structs, eof, eof)
    return cu
}

private fun module(state: ParserState): String {
    var module = ""

    if (match(state, TokenType.MODULE, true)) {
        if (!match(state, TokenType.IDENTIFIER)) error(state, "Expected module name")
        module += next(state).text

        while (match(state, TokenType.DOT, true)) {
            if (!match(state, TokenType.IDENTIFIER))
                error(state, "Expected module name")
            module += "."
            module += next(state).text
        }
    }

    return module
}

private fun import(state: ParserState): ImportNode {
    var importName = ""
    val firstToken = state.current()
    if (match(state, TokenType.IMPORT, true)) {
        if (!match(state, TokenType.IDENTIFIER)) error(state, "Expected namespace identifier")
        importName += next(state).text

        while (match(state, TokenType.DOT, true)) {
            if (!match(state, TokenType.IDENTIFIER)) error(state, "Expected namespace identifier")
            importName += "."
            importName += next(state).text
        }
    }
    return ImportNode(importName, firstToken, state.last())
}

private fun structure(state: ParserState): StructureNode {
    val firstToken = state.current()
    next(state)
    if (!match(state, TokenType.IDENTIFIER)) error(state, "Expected structure name")
    val name = next(state)
    val fields = mutableListOf<VariableDeclarationNode>()
    val functions = mutableListOf<FunctionNode>()
    while (!match(state, TokenType.END)) {
        if (match(state, TokenType.FUNCTION)) functions.add(function(state))
        else if (match(state, TokenType.CONSTRUCTOR)) functions.add(function(state, true))
        else fields.add(variableDeclaration(state, false))
    }
    if (!match(state, TokenType.END, true)) error(state, "Expected end")
    return StructureNode(name, fields, functions, firstToken, state.last())
}


private fun function(state: ParserState, isConstructor: Boolean = false): FunctionNode {
    val firstToken = state.current()
    val extern: Boolean

    if (isConstructor) {
        extern = false
        if (!match(state, TokenType.CONSTRUCTOR)) error(state, "Expected a constructor definition")
    } else {
        extern = match(state, TokenType.EXTERN, true)
        if (!match(state, TokenType.FUNCTION, true)) error(state, "Expected a function definition")
        if (!match(state, TokenType.IDENTIFIER)) error(state, "Expected function name")
    }
    val name: Token = next(state)
    val parameterList = parameterList(state)

    var returnType: TypeNode = NoTypeNode(firstToken, firstToken)
    if (match(state, TokenType.COLON, true)) {
        if (isConstructor) error(state, "Constructors can not return a value")
        returnType = type(state)
    }

    if (extern) return FunctionNode(name, parameterList, returnType, BlockNode(emptyList(), firstToken, state.last()), extern, firstToken, state.last())

    val body = block(state)

    if (!match(state, TokenType.END, true)) error(state, "Expected 'end' at end of function definition")
    return FunctionNode(name, parameterList, returnType, body, extern, firstToken, state.last())
}

private fun parameterList(state: ParserState): List<ParameterNode> {
    val parameters = mutableListOf<ParameterNode>()
    if (!match(state, TokenType.L_PARA, true)) error(state, "Expected a parameter list, starting with (")
    while (!match(state, TokenType.R_PARA, true)) {
        val param = parameter(state)
        parameters.add(param)
        if (!match(state, TokenType.R_PARA)) {
            if (!match(state, TokenType.COMMA, true)) error(state, "Expected a coma (,) between parameters")
        }
    }
    return parameters
}

private fun parameter(state: ParserState): ParameterNode {
    val firstToken = state.current()
    if (!match(state, TokenType.IDENTIFIER)) error(state, "Expected a parameter name")
    val name = next(state)
    if (!match(state, TokenType.COLON, true)) error(state, "Expected a colon (:) between the parameter name and parameter type")
    val type = type(state)
    return ParameterNode(name, type, firstToken, state.last())
}

private fun type(state: ParserState): TypeNode {
    val firstToken = state.current()
    if (!match(state, TokenType.IDENTIFIER)) error(state, "Expected a type")
    val identifiers = mutableListOf<Token>()
    identifiers.add(next(state))

    while (match(state, TokenType.DOT, true)) {
        if (!match(state, TokenType.IDENTIFIER)) error(state, "Expected namespace or type name")
        identifiers.add(next(state))
    }

    // FIXME generics, functions

    var isArray = false
    if (match(state, TokenType.L_BRACK, true)) {
        if (!match(state, TokenType.R_BRACK, true)) error(state, "Expected closing bracket ]")
        isArray = true
    }

    var isOptional = match(state, TokenType.QUESTION, true)

    return TypeNode(identifiers, isArray, isOptional, firstToken, state.last())
}

private fun block(state: ParserState, endTokens: List<TokenType> = listOf(TokenType.END)): BlockNode {
    val firstToken = state.current()
    val statements = mutableListOf<StatementNode>()
    while (!match(state, endTokens)) {
        while (match(state, TokenType.SEMI_COLON, true)) {
        }
        statements.add(statement(state))
        while (match(state, TokenType.SEMI_COLON, true)) {
        }
    }
    return BlockNode(statements, firstToken, state.last())
}


private fun statement(state: ParserState): StatementNode {
    if (match(state, TokenType.VAR)) return variableDeclaration(state)
    else if (match(state, TokenType.RETURN)) return returnStatement(state)
    else if (match(state, TokenType.IF)) return ifStatement(state)
    else if (match(state, TokenType.DO)) return doStatement(state)
    else if (match(state, TokenType.WHILE)) return whileStatement(state)
    else if (match(state, TokenType.FOR)) return forStatement(state)
    else if (match(state, TokenType.BREAK, true)) return BreakNode(state.last(), state.last())
    else if (match(state, TokenType.CONTINUE, true)) return ContinueNode(state.last(), state.last())
    else return expression(state)
}

private fun doStatement(state: ParserState): DoNode {
    val firstToken = state.current()
    if (!match(state, TokenType.DO, true)) error(state, "Expected do")

    val body = block(state, listOf(TokenType.WHILE))
    if (!match(state, TokenType.WHILE, true)) error(state, "Expected while")
    val condition = expression(state)
    if (!match(state, TokenType.END, true)) error(state, "Expected end")
    return DoNode(condition, body, firstToken, state.last())
}

private fun whileStatement(state: ParserState): WhileNode {
    val firstToken = state.current()
    if (!match(state, TokenType.WHILE, true)) error(state, "Expected while")

    val condition = expression(state)
    if (!match(state, TokenType.DO, true)) error(state, "Expected do")

    val body = block(state)
    if (!match(state, TokenType.END, true)) error(state, "Expected end")
    return WhileNode(condition, body, firstToken, state.last())
}

private fun forStatement(state: ParserState): ForNode {
    val firstToken = state.current()
    if (!match(state, TokenType.FOR, true)) error(state, "Expected for")

    val initializer = mutableListOf<VariableDeclarationNode>()
    while (!match(state, TokenType.SEMI_COLON)) {
        initializer.add(variableDeclaration(state, false))
        if (!match(state, TokenType.COMMA, true)) break
    }
    if (!match(state, TokenType.SEMI_COLON, true)) error(state, "Expected semicolon")
    val condition = expression(state)
    if (!match(state, TokenType.SEMI_COLON, true)) error(state, "Expected semicolon")

    val increment = mutableListOf<ExpressionNode>()
    while (!match(state, TokenType.DO)) {
        increment.add(expression(state))
        if (!match(state, TokenType.COMMA, true)) break
    }
    if (!match(state, TokenType.DO, true)) error(state, "Expected do")
    val body = block(state)
    if (!match(state, TokenType.END, true)) error(state, "Expected end")

    return ForNode(initializer, condition, increment, body, firstToken, state.last())
}

val expressionStartTokens = listOf(
        TokenType.L_PARA,
        TokenType.TRUE, TokenType.FALSE, TokenType.NUMBER, TokenType.CHARACTER, TokenType.STRING,
        TokenType.IDENTIFIER
)

private fun returnStatement(state: ParserState): ReturnNode {
    val firstToken = state.current()
    if (!match(state, TokenType.RETURN, true)) error(state, "Expected return")
    if (match(state, expressionStartTokens)) return ReturnNode(expression(state), firstToken, state.last())
    else return ReturnNode(null, firstToken, state.last())
}

private fun variableDeclaration(state: ParserState, expectVar: Boolean = true): VariableDeclarationNode {
    val firstToken = state.current()
    if (expectVar)
        if (!match(state, TokenType.VAR, true)) error(state, "Expected a variable declaration")
    if (!match(state, TokenType.IDENTIFIER)) error(state, "Expected a variable name")
    val name = next(state)

    var type: TypeNode = NoTypeNode(firstToken, firstToken)
    if (match(state, TokenType.COLON, true)) {
        type = type(state)
    }

    var initializer: ExpressionNode? = null
    if (match(state, TokenType.EQUAL, true)) {
        initializer = expression(state)
    }

    return VariableDeclarationNode(name, type, initializer, firstToken, state.last())
}

private fun ifStatement(state: ParserState): IfNode {
    var firstToken = state.current()
    if (!match(state, TokenType.IF, true)) error(state, "Expected if")
    val condition = expression(state)
    if (!match(state, TokenType.THEN, true)) error(state, "Expected then")
    val trueBody = mutableListOf<StatementNode>()
    val elseIfs = mutableListOf<IfNode>()
    val falseBody = mutableListOf<StatementNode>()
    while (!match(state, TokenType.END) and !match(state, TokenType.ELSE) and !match(state, TokenType.ELSEIF)) {
        while (match(state, TokenType.SEMI_COLON, true)) {
        }
        trueBody.add(statement(state))
        while (match(state, TokenType.SEMI_COLON, true)) {
        }
    }
    while (match(state, TokenType.ELSEIF, true)) {
        firstToken = state.last()
        val elseifCondition = expression(state)
        if (!match(state, TokenType.THEN, true)) error(state, "Expected then")
        val elseIfBody = mutableListOf<StatementNode>()
        while (!match(state, TokenType.ELSE) and !match(state, TokenType.ELSEIF) and !match(state, TokenType.END)) {
            while (match(state, TokenType.SEMI_COLON, true)) {
            }
            elseIfBody.add(statement(state))
            while (match(state, TokenType.SEMI_COLON, true)) {
            }
        }
        elseIfs.add(IfNode(elseifCondition, BlockNode(elseIfBody, firstToken, state.last()), emptyList<IfNode>(), BlockNode(emptyList(), firstToken, state.last()), firstToken, state.last()))
    }
    if (match(state, TokenType.ELSE, true)) {
        while (match(state, TokenType.SEMI_COLON, true)) {
        }
        falseBody.add(statement(state))
        while (match(state, TokenType.SEMI_COLON, true)) {
        }
    }
    if (!match(state, TokenType.END, true)) error(state, "Expected end")
    return IfNode(condition, BlockNode(trueBody, firstToken, state.last()), elseIfs, BlockNode(falseBody, firstToken, state.last()), firstToken, state.last())
}

private val binaryOpGroups = listOf(
        listOf(TokenType.EQUAL, TokenType.PLUS_EQUAL, TokenType.MINUS_EQUAL, TokenType.MUL_EQUAL, TokenType.DIV_EQUAL, TokenType.MOD_EQUAL),
        listOf(TokenType.OR, TokenType.AND, TokenType.XOR),
        listOf(TokenType.DOUBLE_EQUAL, TokenType.NOT_EQUAL, TokenType.TRIPLE_EQUAL),
        listOf(TokenType.LESS, TokenType.LESSEQUAL, TokenType.GREATER, TokenType.GREATEREQUAL),
        listOf(TokenType.SHL, TokenType.SHR),
        listOf(TokenType.PLUS, TokenType.MINUS),
        listOf(TokenType.DIV, TokenType.MUL, TokenType.MODULO)
)

private val unaryOperators = listOf(TokenType.MINUS, TokenType.PLUS, TokenType.NOT)

private fun expression(state: ParserState): ExpressionNode {
    return ternaryOperator(state)
}

private fun ternaryOperator(state: ParserState): ExpressionNode {
    val firstToken = state.current()
    val left = binaryOperator(state, 0)

    if (match(state, TokenType.QUESTION, true)) {
        val middle = ternaryOperator(state)
        if (!match(state, TokenType.COLON, true)) error(state, "Expected colon for ternary operator")
        val right = ternaryOperator(state)
        return TernaryOperatorNode(left, middle, right, firstToken, state.last())
    } else {
        return left
    }
}

private fun binaryOperator(state: ParserState, level: Int): ExpressionNode {
    var firstToken = state.current()
    val nextLevel = level + 1
    var left = if (nextLevel == binaryOpGroups.size) unaryOperator(state) else binaryOperator(state, nextLevel)

    val ops = binaryOpGroups[level]
    while (match(state, ops)) {
        val opType = next(state)
        val right = if (nextLevel == binaryOpGroups.size) unaryOperator(state) else binaryOperator(state, nextLevel)
        left = BinaryOperatorNode(opType, left, right, firstToken, state.last())
        firstToken = state.current()
    }

    return left
}

private fun unaryOperator(state: ParserState): ExpressionNode {
    var firstToken = state.tokens[state.currentToken]
    if (match(state, unaryOperators)) {
        return UnaryOperatorNode(next(state), unaryOperator(state), firstToken, state.last())
    } else {
        return factor(state)
    }
}

private fun factor(state: ParserState): ExpressionNode {
    var firstToken = state.current()

    // match paranthesised expressions
    if (match(state, TokenType.L_PARA, true)) {
        val expr = expression(state)
        if (!match(state, TokenType.R_PARA, true)) error(state, "Expected closing paranthesis )")
        return ParenthesisNode(expr, firstToken, state.last())
    }

    // match literals
    if (match(state, TokenType.NULL, true)) {
        return NullLiteralNode(firstToken, state.last())
    }

    if (match(state, TokenType.TRUE) || match(state, TokenType.FALSE)) {
        return BooleanLiteralNode(next(state), firstToken, state.last())
    }
    if (match(state, TokenType.NUMBER)) {
        return NumberLiteralNode(next(state), firstToken, state.last())
    }
    if (match(state, TokenType.CHARACTER)) {
        return CharacterLiteralNode(next(state), firstToken, state.last())
    }
    if (match(state, TokenType.STRING)) {
        return StringLiteralNode(next(state), firstToken, state.last())
    }

    // match variable access, field access, array access and function calls
    if (match(state, TokenType.IDENTIFIER)) {
        var lastExpr: ExpressionNode = VariableAccessNode(next(state), firstToken, state.last())

        while (true) {
            // function or method call
            if (match(state, TokenType.L_PARA, true)) {
                firstToken = state.last()
                val args = mutableListOf<ExpressionNode>()
                if (!match(state, TokenType.R_PARA)) {
                    while (true) {
                        args.add(expression(state))
                        if (!match(state, TokenType.COMMA, true)) break
                    }
                    if (!match(state, TokenType.R_PARA, true)) error(state, "Expected closing paranthesis )")
                } else {
                    if (!match(state, TokenType.R_PARA, true)) error(state, "Expected closing paranthesis )")
                }
                lastExpr = FunctionCallNode(lastExpr, args, firstToken, state.last())
                continue
            }

            // array access
            if (match(state, TokenType.L_BRACK, true)) {
                firstToken = state.last()
                val expr = expression(state)
                if (!match(state, TokenType.R_BRACK, true)) error(state, "Expected closing bracket ]")
                lastExpr = ArrayAccessNode(lastExpr, expr, firstToken, state.last())
                continue
            }

            // field access
            if (match(state, TokenType.DOT, true)) {
                if (!match(state, TokenType.IDENTIFIER)) error(state, "Expected field name")
                lastExpr = FieldAccessNode(lastExpr, next(state), state.last(), state.last())
                continue
            }
            break
        }
        return lastExpr
    }

    error(state, "Expected a boolean, number, character, string, variable name or function call")
    throw RuntimeException("Unreachable")
}