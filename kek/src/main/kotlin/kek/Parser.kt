package kek

import java.sql.Struct

private data class ParserState(val input: CharSequence = "",
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

private fun compilationUnit(state: ParserState): CompilationUnit {
    val nameSpace = nameSpace(state)
    val imports = mutableListOf<Import>()
    val functions = mutableListOf<FunctionDefinition>()
    val structs = mutableListOf<StructureDefinition>()

    while (!match(state, TokenType.EOF)) {
        if (match(state, TokenType.FUNCTION)) {
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
        CompilationUnit(nameSpace, imports, functions, structs, state.tokens.first(), state.tokens.last())
    else
        CompilationUnit(nameSpace, imports, functions, structs, eof, eof)
    return cu
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

private fun import(state: ParserState): Import {
    var importName = ""
    val firstToken = state.current()
    if (match(state, TokenType.IMPORT, true)) {
        if (!match(state, TokenType.IDENTIFIER)) error("Expected namespace identifier")
        importName += next(state).text

        while (match(state, TokenType.DOT, true)) {
            if (!match(state, TokenType.IDENTIFIER)) error("Expected namespace identifier")
            importName += "."
            importName += next(state).text
        }
    }
    return Import(importName, firstToken, state.last())
}

private fun structure(state: ParserState): StructureDefinition {
    val firstToken = state.current()
    next(state)
    if (!match(state, TokenType.IDENTIFIER)) error(state, "Expected structure name")
    val name = next(state)
    val fields = mutableListOf<VariableDeclaration>()
    while (!match(state, TokenType.END)) {
        fields.add(variableDeclaration(state, false))
    }
    if (!match(state, TokenType.END, true)) error(state, "Expected end")
    return StructureDefinition(name, fields, firstToken, state.last())
}


private fun function(state: ParserState): FunctionDefinition {
    val firstToken = state.current()
    if (!match(state, TokenType.FUNCTION, true)) error(state, "Expected a function definition")
    if (!match(state, TokenType.IDENTIFIER)) error(state, "Expected function name")
    val name: Token = next(state)
    val parameterList = parameterList(state)

    var returnType: Type = NoType(firstToken, firstToken)
    if (match(state, TokenType.COLON, true)) {
        returnType = type(state)
    }

    val body = functionBody(state)

    if (!match(state, TokenType.END, true)) error(state, "Expected 'end' at end of function definition")
    return FunctionDefinition(name, parameterList, returnType, body, firstToken, state.last())
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
    return parameters
}

private fun parameter(state: ParserState): Parameter {
    val firstToken = state.current()
    if (!match(state, TokenType.IDENTIFIER)) error(state, "Expected a parameter name")
    val name = next(state)
    if (!match(state, TokenType.COLON, true)) error(state, "Expected a colon (:) between the parameter name and parameter type")
    val type = type(state)
    return Parameter(name, type, firstToken, state.last())
}

private fun type(state: ParserState): Type {
    val firstToken = state.current()
    if (!match(state, TokenType.IDENTIFIER)) error(state, "Expected a type")
    val identifiers = mutableListOf<Token>()
    identifiers.add(next(state))

    while (match(state, TokenType.DOT, true)) {
        if (!match(state, TokenType.IDENTIFIER)) error(state, "Expected namespace or type name");
        identifiers.add(next(state));
    }

    // FIXME generics, functions

    var isArray = false
    if (match(state, TokenType.L_BRACK, true)) {
        if (!match(state, TokenType.R_BRACK, true)) error(state, "Expected closing bracket ]");
        isArray = true
    }

    var isOptional = match(state, TokenType.QUESTION, true)
    return Type(identifiers, isArray, isOptional, firstToken, state.last())
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
    if (match(state, TokenType.FOR)) return forStatement(state)
    if (match(state, TokenType.WHILE)) return whileStatement(state)
    if (match(state, TokenType.DO)) return doStatement(state)
    if (match(state, TokenType.BREAK, true)) return BreakStatement(state.last(), state.last());
    if (match(state, TokenType.CONTINUE, true)) return ContinueStatement(state.last(), state.last());
    else return expression(state)
}

private fun doStatement(state: ParserState): Statement {
    val firstToken = state.current()
    if (!match(state, TokenType.DO, true)) error("Expected do")

    val body = mutableListOf<Statement>()
    while (!match(state, TokenType.WHILE)) {
        while (match(state, TokenType.SEMI_COLON, true)) {
        }
        body.add(statement(state))
        while (match(state, TokenType.SEMI_COLON, true)) {
        }
    }
    if (!match(state, TokenType.WHILE, true)) error("Expected while")
    val condition = expression(state);
    if (!match(state, TokenType.END, true)) error("Expected end")
    return DoStatement(condition, body, firstToken, state.last())
}

private fun whileStatement(state: ParserState): Statement {
    val firstToken = state.current()
    if (!match(state, TokenType.WHILE, true)) error("Expected while")

    val condition = expression(state)
    if (!match(state, TokenType.DO, true)) error("Expected do")

    val body = mutableListOf<Statement>()
    while (!match(state, TokenType.END)) {
        while (match(state, TokenType.SEMI_COLON, true)) {
        }
        body.add(statement(state))
        while (match(state, TokenType.SEMI_COLON, true)) {
        }
    }
    if (!match(state, TokenType.END, true)) error("Expected end")
    return WhileStatement(condition, body, firstToken, state.last())
}

private fun forStatement(state: ParserState): Statement {
    val firstToken = state.current()
    if (!match(state, TokenType.FOR, true)) error("Expected for")

    val initializer = mutableListOf<VariableDeclaration>()
    while (!match(state, TokenType.SEMI_COLON)) {
        initializer.add(variableDeclaration(state, false))
        if (!match(state, TokenType.COMMA, true)) break
    }
    if (!match(state, TokenType.SEMI_COLON, true)) error(state, "Expected semicolon")
    val condition = expression(state)
    if (!match(state, TokenType.SEMI_COLON, true)) error(state, "Expected semicolon")

    val increment = mutableListOf<Expression>()
    while (!match(state, TokenType.DO)) {
        increment.add(expression(state))
        if (!match(state, TokenType.COMMA, true)) break
    }
    if (!match(state, TokenType.DO, true)) error(state, "Expected do")
    val body = mutableListOf<Statement>()
    while (!match(state, TokenType.END) and !match(state, TokenType.ELSE)) {
        while (match(state, TokenType.SEMI_COLON, true)) {
        }
        body.add(statement(state))
        while (match(state, TokenType.SEMI_COLON, true)) {
        }
    }
    if (!match(state, TokenType.END, true)) error(state, "Expected end")

    return ForStatement(initializer, condition, increment, body, firstToken, state.last())
}

private fun returnStatement(state: ParserState): Statement {
    val firstToken = state.current()
    if (!match(state, TokenType.RETURN, true)) error("Expected return")
    return ReturnStatement(expression(state), firstToken, state.last())
}

private fun variableDeclaration(state: ParserState, expectVar: Boolean = true): VariableDeclaration {
    val firstToken = state.current()
    if (expectVar)
        if (!match(state, TokenType.VAR, true)) error(state, "Expected a variable declaration")
    if (!match(state, TokenType.IDENTIFIER)) error(state, "Expected a variable name")
    val name = next(state)

    var type: Type = NoType(firstToken, firstToken)
    if (match(state, TokenType.COLON, true)) {
        type = type(state)
    }

    var initializer: Expression = EmptyExpression(firstToken, state.last())
    if (match(state, TokenType.EQUAL, true)) {
        initializer = expression(state)
    }

    return VariableDeclaration(name, type, initializer, firstToken, state.last())
}

private fun ifStatement(state: ParserState): Statement {
    val firstToken = state.current()
    if (!match(state, TokenType.IF, true)) error("Expected if")
    val condition = expression(state)
    if (!match(state, TokenType.THEN, true)) error("Expected then")
    val trueBody = mutableListOf<Statement>()
    val elseIfs = mutableListOf<IfStatement>()
    val falseBody = mutableListOf<Statement>()
    while (!match(state, TokenType.END) and !match(state, TokenType.ELSE) and !match(state, TokenType.ELSEIF)) {
        while (match(state, TokenType.SEMI_COLON, true)) {
        }
        trueBody.add(statement(state))
        while (match(state, TokenType.SEMI_COLON, true)) {
        }
    }
    while (match(state, TokenType.ELSEIF, true)) {
        var firstToken = state.last()
        val elseifCondition = expression(state)
        if (!match(state, TokenType.THEN, true)) error("Expected then")
        val elseIfBody = mutableListOf<Statement>()
        while (!match(state, TokenType.ELSE) and !match(state, TokenType.ELSEIF) and !match(state, TokenType.END)) {
            while (match(state, TokenType.SEMI_COLON, true)) {
            }
            elseIfBody.add(statement(state))
            while (match(state, TokenType.SEMI_COLON, true)) {
            }
        }
        elseIfs.add(IfStatement(elseifCondition, elseIfBody, emptyList<IfStatement>(), emptyList<Statement>(), firstToken, state.last()))
    }
    if (match(state, TokenType.ELSE, true)) {
        while (match(state, TokenType.SEMI_COLON, true)) {
        }
        falseBody.add(statement(state))
        while (match(state, TokenType.SEMI_COLON, true)) {
        }
    }
    if (!match(state, TokenType.END, true)) error("Expected end")
    return IfStatement(condition, trueBody, elseIfs, falseBody, firstToken, state.last())
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

private fun expression(state: ParserState): Expression {
    return ternaryOperator(state)
}

private fun ternaryOperator(state: ParserState): Expression {
    val firstToken = state.current()
    val left = binaryOperator(state, 0)

    if (match(state, TokenType.QUESTION, true)) {
        val middle = ternaryOperator(state);
        if (!match(state, TokenType.COLON, true)) error("Expected colon for ternary operator")
        val right = ternaryOperator(state);
        return TernaryOperator(left, middle, right, firstToken, state.last());
    } else {
        return left;
    }
}

private fun binaryOperator(state: ParserState, level: Int): Expression {
    var firstToken = state.current()
    val nextLevel = level + 1
    var left = if (nextLevel == binaryOpGroups.size) unaryOperator(state) else binaryOperator(state, nextLevel)

    val ops = binaryOpGroups[level];
    while (match(state, ops)) {
        val opType = next(state)
        val right = if (nextLevel == binaryOpGroups.size) unaryOperator(state) else binaryOperator(state, nextLevel)
        left = BinaryOperator(opType, left, right, firstToken, state.last())
        firstToken = state.current()
    }

    return left
}

private fun unaryOperator(state: ParserState): Expression {
    var firstToken = state.tokens[state.currentToken]
    if (match(state, unaryOperators)) {
        return UnaryOperator(next(state), unaryOperator(state), firstToken, state.last())
    } else {
        return factor(state)
    }
}

private fun factor(state: ParserState): Expression {
    var firstToken = state.current()

    // match paranthesised expressions
    if (match(state, TokenType.L_PARA, true)) {
        val expr = expression(state)
        if (!match(state, TokenType.R_PARA, true)) error(state, "Expected closing paranthesis )")
        return Parenthesis(expr, firstToken, state.last())
    }

    // match literals
    if (match(state, TokenType.TRUE) or match(state, TokenType.FALSE)) {
        return BooleanLiteral(next(state), firstToken, state.last());
    }
    if (match(state, TokenType.NUMBER)) {
        return NumberLiteral(next(state), firstToken, state.last())
    }
    if (match(state, TokenType.CHARACTER)) {
        return CharacterLiteral(next(state), firstToken, state.last())
    }
    if (match(state, TokenType.STRING)) {
        return StringLiteral(next(state), firstToken, state.last())
    }

    // match variable access, field access, array access and function calls
    if (match(state, TokenType.IDENTIFIER)) {
        var lastExpr: Expression = VariableAccess(next(state), firstToken, state.last())

        while (true) {
            // function or method call
            if (match(state, TokenType.L_PARA, true)) {
                firstToken = state.last()
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
                lastExpr = FunctionCall(lastExpr, args, firstToken, state.last())
                continue
            }

            // array access
            if (match(state, TokenType.L_BRACK, true)) {
                firstToken = state.last()
                val expr = expression(state)
                if (!match(state, TokenType.R_BRACK, true)) error(state, "Expected closing bracket ]")
                lastExpr = ArrayAccess(lastExpr, expr, firstToken, state.last())
                continue
            }

            // field access
            if (match(state, TokenType.DOT, true)) {
                if (!match(state, TokenType.IDENTIFIER)) error(state, "Expected field name")
                lastExpr = FieldAccess(lastExpr, next(state), state.last(), state.last())
                continue
            }
            break
        }
        return lastExpr
    }

    error(state, "Expected a number, character, string, variable name or function call")
    return EmptyExpression(firstToken, state.last())
}