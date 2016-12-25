package kek

private data class ParserState(val input: CharSequence = "",
                               val tokens: List<Token>,
                               var currentToken: Int = 0,
                               val nodeStack: MutableList<AstNode> = mutableListOf<AstNode>())

fun parse(input: CharSequence): CompilationUnit {
    val tokens = tokenize(input)
    val state = ParserState(input, tokens)
    return compilationUnit(state)
}

private fun match(state: ParserState, tokenTypes: List<TokenType>): Boolean {
    for (type in tokenTypes) {
        if (match(state, type)) return true;
    }
    return false;
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
    throw CompilerException(state.input.toString(), msg + ", got " + token.text,
            token.line, token.column, token.column + token.text.length)
}

private fun nameSpace(state: ParserState): String {
    var nameSpace = ""

    if (match(state, TokenType.NAMESPACE, true)) {
        if (!match(state, TokenType.IDENTIFIER)) error("Expected namespace identifier")
        nameSpace += next(state).text

        while (match(state, TokenType.DOT, true)) {
            if (!match(state, TokenType.IDENTIFIER)) error("Expected namespace identifier")
            nameSpace += "."
            nameSpace += next(state).text
        }
    }

    return nameSpace
}

private fun compilationUnit(state: ParserState): CompilationUnit {
    val nameSpace = nameSpace(state)
    val functions = mutableListOf<FunctionDefinition>()
    val structs = mutableListOf<StructureDefinition>()

    while (!match(state, TokenType.EOF)) {
        if (match(state, TokenType.FUNCTION)) {
            functions.add(function(state))
        } else if (match(state, TokenType.STRUCTURE)) {

        } else {
            error(state, "Expected a function or structure definition")
        }
    }

    // FIXME
    return CompilationUnit(nameSpace, functions, structs)
}


private fun function(state: ParserState): FunctionDefinition {
    if (!match(state, TokenType.FUNCTION, true)) error(state, "Expected a function definition")
    if (!match(state, TokenType.IDENTIFIER)) error(state, "Expected function name")
    val name: Token = next(state)
    val parameterList = parameterList(state)

    var returnType: Type = NoType()
    if (match(state, TokenType.COLON, true)) {
        returnType = type(state)
    }

    val body = functionBody(state)

    if (!match(state, TokenType.END, true)) error(state, "Expected 'end' at end of function definition")

    return FunctionDefinition(name, parameterList, returnType, body)
}

private fun parameterList(state: ParserState): List<Parameter> {
    val parameters = mutableListOf<Parameter>()
    if (!match(state, TokenType.L_PARA, true)) error(state, "Expected a parameter list, starting with (")
    while (!match(state, TokenType.R_PARA, true)) {
        val param = parameter(state)
        parameters.add(param)
        if (!match(state, TokenType.R_PARA)) {
            if (!match(state, TokenType.COMMA, true)) error(state, "Expected a coma (,) between parameters")
        }
    }

    // FIXME
    return parameters
}

private fun parameter(state: ParserState): Parameter {
    if (!match(state, TokenType.IDENTIFIER)) error(state, "Expected a parameter name")
    val name = next(state)
    if (!match(state, TokenType.COLON, true)) error(state, "Expected a colon (:) between the parameter name and parameter type")
    val type = type(state)

    return Parameter(name, type)
}

private fun type(state: ParserState): Type {
    if (!match(state, TokenType.IDENTIFIER)) error(state, "Expected a type")
    val typeIdentifier = next(state)

    // FIXME array types, generics, fully qualified names
    return Type(typeIdentifier)
}

private fun functionBody(state: ParserState): List<Statement> {
    val statements = mutableListOf<Statement>()
    while (!match(state, TokenType.END)) {
        while (match(state, TokenType.SEMI_COLON, true)) {
        }
        statements.add(statement(state))
        while (match(state, TokenType.SEMI_COLON, true)) {
        }
    }
    return statements
}


private fun statement(state: ParserState): Statement {
    if (match(state, TokenType.VAR)) return variableDeclaration(state)
    if (match(state, TokenType.RETURN)) return returnStatement(state)
    if (match(state, TokenType.IF)) return ifStatement(state)
    // FIXME
    // if (match(state, TokenType.FOR)) return forStatement(state)
    // if (match(state, TokenType.WHILE)) return WhileStatement(state)
    // if (match(state, TokenType.DO)) return DoStatement(state)
    return expression(state)
}

private fun returnStatement(state: ParserState): Statement {
    next(state);
    return ReturnStatement(expression(state));
}

private fun variableDeclaration(state: ParserState): VariableDeclaration {
    if (!match(state, TokenType.VAR, true)) error(state, "Expected a variable declaration")
    if (!match(state, TokenType.IDENTIFIER)) error(state, "Expected a variable name")
    val name = next(state)

    var type: Type = NoType()
    if (match(state, TokenType.COLON, true)) {
        type = type(state)
    }

    var initializer: Expression = EmptyExpression()
    if (match(state, TokenType.EQUAL, true)) {
        initializer = expression(state)
    }

    return VariableDeclaration(name, type, initializer)
}

private fun ifStatement(state: ParserState): Statement {
    next(state)
    val condition = expression(state)
    if (!match(state, TokenType.THEN, true)) error("Expected then")
    val trueBody = mutableListOf<Statement>()
    val falseBody = mutableListOf<Statement>()
    while (!match(state, TokenType.END) and !match(state, TokenType.ELSE)) {
        while (match(state, TokenType.SEMI_COLON, true)) {
        }
        trueBody.add(statement(state))
        while (match(state, TokenType.SEMI_COLON, true)) {
        }
    }
    if (match(state, TokenType.ELSE, true)) {
        while (match(state, TokenType.SEMI_COLON, true)) {
        }
        falseBody.add(statement(state))
        while (match(state, TokenType.SEMI_COLON, true)) {
        }
    }
    if (!match(state, TokenType.END, true)) error("Expected end")
    return IfStatement(condition, trueBody, falseBody)
}

private val binaryOpGroups = listOf(
        listOf(TokenType.EQUAL),
        listOf(TokenType.OR, TokenType.AND, TokenType.XOR),
        listOf(TokenType.DOUBLE_EQUAL, TokenType.NOT_EQUAL, TokenType.TRIPLE_EQUAL),
        listOf(TokenType.LESS, TokenType.LESSEQUAL, TokenType.GREATER, TokenType.GREATEREQUAL),
        listOf(TokenType.SHL, TokenType.SHR),
        listOf(TokenType.PLUS, TokenType.MINUS),
        listOf(TokenType.DIV, TokenType.MUL, TokenType.MODULO)
)

private val unaryOperators = listOf(TokenType.MINUS, TokenType.PLUS, TokenType.NOT)

private fun expression(state: ParserState): Expression {
    return ternaryOperator(state);
}

private fun ternaryOperator(state: ParserState): Expression {
    val left = binaryOperator(state, 0)

    if (match(state, TokenType.QUESTION, true)) {
        val middle = ternaryOperator(state);
        if (!match(state, TokenType.COLON, true)) error("Expected colon for ternary operator")
        val right = ternaryOperator(state);
        return TernaryOperator(left, middle, right);
    } else {
        return left;
    }
}

private fun binaryOperator(state: ParserState, level: Int): Expression {
    val nextLevel = level + 1
    val left = if (nextLevel == binaryOpGroups.size) unaryOperator(state) else binaryOperator(state, nextLevel)

    val ops = binaryOpGroups[level];
    if (match(state, ops)) {
        val opType = next(state)
        val right = if (nextLevel == binaryOpGroups.size) unaryOperator(state) else binaryOperator(state, nextLevel)
        return BinaryOperator(opType, left, right)
    } else {
        return left
    }
}

private fun unaryOperator(state: ParserState): Expression {
    if (match(state, unaryOperators)) {
        return UnaryOperator(next(state), unaryOperator(state))
    } else {
        return factor(state)
    }
}

private fun factor(state: ParserState): Expression {
    // match paranthesised expressions
    if (match(state, TokenType.L_PARA, true)) {
        val expr = expression(state)
        if (!match(state, TokenType.R_PARA, true)) error(state, "Expected closing paranthesis )")
        return expr
    }

    // match literals
    if (match(state, TokenType.TRUE) or match(state, TokenType.FALSE)) {
        return BooleanLiteral(next(state));
    }
    if (match(state, TokenType.NUMBER)) {
        return NumberLiteral(next(state))
    }
    if (match(state, TokenType.CHARACTER)) {
        return CharacterLiteral(next(state))
    }
    if (match(state, TokenType.STRING)) {
        return StringLiteral(next(state))
    }

    // match variable access, field access, array access and function calls
    if (match(state, TokenType.IDENTIFIER)) {
        var lastExpr: Expression = VariableAccess(next(state))

        while (true) {
            // function or method call
            if (match(state, TokenType.L_PARA, true)) {
                val args = mutableListOf<Expression>()
                if (!match(state, TokenType.R_PARA)) {
                    while (true) {
                        args.add(expression(state))
                        if (!match(state, TokenType.COMMA)) break
                        else next(state)
                    }
                    if (!match(state, TokenType.R_PARA, true)) error(state, "Expected closing paranthesis )")
                } else {
                    if (!match(state, TokenType.R_PARA, true)) error(state, "Expected closing paranthesis )")
                }
                lastExpr = FunctionCall(lastExpr, args)
                continue
            }

            // array access
            if (match(state, TokenType.L_BRACK, true)) {
                lastExpr = ArrayAccess(lastExpr, expression(state))
                if (!match(state, TokenType.R_BRACK, true)) error(state, "Expected closing bracket ]")
                continue
            }

            // field access
            if (match(state, TokenType.DOT, true)) {
                if (!match(state, TokenType.IDENTIFIER)) error(state, "Expected field name")
                lastExpr = FieldAccess(lastExpr, next(state))
                continue
            }
            break
        }
        return lastExpr
    }

    error(state, "Expected a number, character, string, variable name or function call")
    return EmptyExpression()
}