package kek.compiler

import kek.runtime.*

class ModuleTypeLookup(val modules: List<Module>) {
    fun lookup(name: String): List<TypeInfo> {
        val result = mutableListOf<TypeInfo>()
        for (m in modules) {
            result.addAll(m.lookup(name))
        }
        return result
    }

    fun lookupFunction(name: String): List<FunctionType> {
        val result = mutableListOf<FunctionType>()
        for (m in modules) {
            result.addAll(m.lookupFunction(name))
        }
        return result
    }

    fun lookupPrimitiveOrStructure(name: String): List<TypeInfo> {
        val result = mutableListOf<TypeInfo>()
        for (m in modules) {
            result.addAll(m.lookupPrimitiveOrStructure(name))
        }
        return result
    }
}

enum class VariableType {
    Global,
    Local,
    Parameter
}

class Variable(val location: Token, val name: String, val variableType: VariableType, val type: TypeInfo)

class VariableLookup {
    private val variablesStack = mutableListOf<MutableMap<String, Variable>>()

    constructor() {
        pushScope()
    }

    fun pushScope() {
        variablesStack.add(mutableMapOf())
    }

    fun popScope() {
        variablesStack.removeAt(variablesStack.lastIndex)
    }

    fun add(symbol: Variable) {
        variablesStack.last().put(symbol.name, symbol)
    }

    fun lookup(name: String): Variable? {
        for (i in variablesStack.lastIndex downTo 0) {
            val variables = variablesStack[i]
            if (variables.containsKey(name))
                return variables[name]!!
        }
        return null
    }
}

class CompilerException(val source: Source, val msg: String, val line: Int, val columnStart: Int, val columnEnd: Int) : RuntimeException(msg) {

    constructor(source: Source, msg: String, token: Token) : this(source, msg, token.line, token.column, token.column + token.text.length) {
    }

    override fun toString(): String {
        val lines = source.sourceCode.split('\n')
        if (!lines.isEmpty()) {
            return "Error ${source.location}:(${line}, ${columnStart}): ${msg} ${lines[line - 1].substring(columnStart - 1)}"
        } else {
            return "Error: ${msg}"
        }
    }
}

data class CompilerState(val compilationUnits: MutableList<CompilationUnitNode> = mutableListOf<CompilationUnitNode>(),
                         val modules: MutableMap<String, Module> = mutableMapOf<String, Module>()) {
}

fun compile(sources: List<Source>): CompilerState {
    val state = CompilerState()
    val defaultModule = Module("")
    defaultModule.primitiveTypes.put(Int8Type.name, Int8Type)
    defaultModule.primitiveTypes.put(Int16Type.name, Int16Type)
    defaultModule.primitiveTypes.put("int", Int32Type)
    defaultModule.primitiveTypes.put(Int32Type.name, Int32Type)
    defaultModule.primitiveTypes.put(Int64Type.name, Int64Type)
    defaultModule.primitiveTypes.put(FloatType.name, FloatType)
    defaultModule.primitiveTypes.put(DoubleType.name, DoubleType)
    defaultModule.primitiveTypes.put(BooleanType.name, BooleanType)
    defaultModule.primitiveTypes.put(VoidType.name, VoidType)
    state.modules.put("", defaultModule)

    for (source in sources) {
        state.compilationUnits.add(parse(source))
    }

    gatherModules(state)
    resolveTopLevelTypes(state)
    resolveAllTypes(state)

    return state
}

/**
 * Creates modules for every compilation unit and gathers
 * all structures and functions. The structures and functions aren't type
 * initialized after this point. See resolveTypes().
 */
private fun gatherModules(state: CompilerState) {
    for (cu in state.compilationUnits) {
        val module: Module
        if (state.modules.containsKey(cu.module)) {
            module = state.modules[cu.module]!!
        } else {
            module = Module(cu.module)
            state.modules[cu.module] = module
        }

        cu.setAnnotation(module, Module::class.java)

        for (i in cu.imports) {
            module.imports.add(i.importName)
        }

        for (s in cu.structs) {
            if (module.structures.containsKey(s.name.text)) {
                val otherStruct = module.structures[s.name.text]!!
                throw CompilerException(cu.source, "Structure ${module.name}.${s.name.text} already defined in ${otherStruct.location.source.location}:(${otherStruct.location.line}, ${otherStruct.location.column})", s.name)
            }

            val struct = StructureType(Location(cu.source, s.name.line, s.name.column), module.name, s.name.text)
            s.setAnnotation(struct, StructureType::class.java)
            module.structures[struct.name] = struct
        }

        for (f in cu.functions) {
            val func = FunctionType(Location(cu.source, f.name.line, f.name.column), module.name, f.name.text, f.extern)
            var funcs = module.functions[func.name]
            if (funcs == null) {
                funcs = mutableListOf()
                module.functions[func.name] = funcs
            }
            f.setAnnotation(func, kek.runtime.FunctionType::class.java)
            funcs.add(func)
        }
    }
}

/**
 * Resolves the types referenced by structures and functions.
 * Does not resolve types referenced in function bodies.
 */
private fun resolveTopLevelTypes(state: CompilerState) {
    for (cu in state.compilationUnits) {
        val importedModules = mutableListOf<Module>()
        importedModules.add(state.modules[""]!!)
        importedModules.add(cu.getAnnotation(Module::class.java))
        for (i in cu.imports) {
            if (!state.modules.containsKey(i.importName)) throw CompilerException(cu.source, "Could not find imported module '${i.importName}'", i.firstToken.line, i.firstToken.column, i.firstToken.column + i.importName.length)
            importedModules.add(state.modules[i.importName]!!)
        }
        val lookup = ModuleTypeLookup(importedModules)

        traverseAstDepthFirst(cu, object : AstVisitorAdapter() {
            override fun structure(n: StructureNode) {
                val structure = n.getAnnotation(StructureType::class.java)

                // resolve types of fields
                for (field in n.fields) {
                    // fields must have a type given, infering the type right the initializer is left hard at the moment
                    if (field.type is NoTypeNode) {
                        throw CompilerException(cu.source, "No type given for field '${field.name.text}'", field.name)
                    } else {
                        structure.fields.add(NamedType(field.name.text, typeNodeToType(lookup, cu.source, "field '${field.name.text}'", field.firstToken, field.type)))
                    }
                }
            }

            override fun function(n: FunctionNode) {
                val function = n.getAnnotation(FunctionType::class.java)

                // resolve types of parameters
                for (parameter in n.parameters) {
                    val type = NamedType(parameter.name.text, typeNodeToType(lookup, cu.source, "parameter '${parameter.name.text}'", parameter.firstToken, parameter.type))
                    function.parameters.add(type)
                    parameter.setAnnotation(type.type, TypeInfo::class.java)
                }

                // resolve type of return value
                if (n.returnType is NoTypeNode) function.returnType = VoidType
                else function.returnType = typeNodeToType(lookup, cu.source, "return value", n.name, n.returnType)
            }
        }, setOf<Class<out AstNode>>(CompilationUnitNode::class.java, StructureNode::class.java, FunctionNode::class.java))
    }
}

private fun typeNodeToType(lookup: ModuleTypeLookup, source: Source, typeOfWhat: String, token: Token, node: TypeNode): TypeInfo {
    val candidateTypes = lookup.lookupPrimitiveOrStructure(node.fullyQualfiedName())
    if (candidateTypes.size == 1) {
        var type = candidateTypes[0]
        if (node.isArray) type = ArrayType(type)
        if (node.isOptional) type = OptionalType(type)
        return type
    } else if (candidateTypes.size > 1) {
        throw CompilerException(source, "Multiple types matching type of $typeOfWhat", token)
    } else {
        throw CompilerException(source, "Could not find type '${node.fullyQualfiedName()}' of $typeOfWhat", token)
    }
}

/**
 * Assigns types to all nodes by setting node.setAnnotation(type, TypeInfo::class.java) and associates
 * break and continue statements with their enclosing loops by setting node.setAnnotation(loop, AstNode::class.java).
 */
private fun resolveAllTypes(state: CompilerState) {
    for (cu in state.compilationUnits) {
        val importedModules = mutableListOf<Module>()
        importedModules.add(state.modules[""]!!)
        importedModules.add(cu.getAnnotation(Module::class.java))
        for (i in cu.imports) {
            if (!state.modules.containsKey(i.importName)) throw CompilerException(cu.source, "Could not find imported module '${i.importName}'", i.firstToken.line, i.firstToken.column, i.firstToken.column + i.importName.length)
            importedModules.add(state.modules[i.importName]!!)
        }
        val typeLookup = ModuleTypeLookup(importedModules)
        val variableLookup = VariableLookup()

        traverseAstDepthFirst(cu, object : AstVisitorAdapter() {
            var currentFunction: FunctionType? = null
            val loopStack = mutableListOf<AstNode>()

            override fun pushScope(n: AstNode) {
                variableLookup.pushScope()
                when (n) {
                    is FunctionNode -> {
                        currentFunction = n.getAnnotation(FunctionType::class.java)
                        for (p in n.parameters) {
                            val pt = p.getAnnotation(TypeInfo::class.java)
                            variableLookup.add(Variable(p.name, p.name.text, VariableType.Parameter, pt))
                        }
                    }
                }
            }

            override fun popScope(n: AstNode) {
                if (n is FunctionNode) currentFunction = null
                variableLookup.popScope()
            }

            override fun pushLoop(n: AstNode) {
                loopStack.add(n)
            }

            override fun popLoop(n: AstNode) {
                loopStack.removeAt(loopStack.lastIndex)
            }

            override fun variableDeclaration(n: VariableDeclarationNode) {
                val variable = variableLookup.lookup(n.name.text)
                if (variable != null) throw CompilerException(cu.source, "Variable '${n.name.text}' already defined at (${variable.location.line}:${variable.location.column})", n.name)
                val type: TypeInfo
                if (n.type is NoTypeNode) {
                    if (n.initializer == null) throw CompilerException(cu.source, "Variable '${n.name.text}' needs a type or initializer", n.name)
                    type = n.initializer.getAnnotation(TypeInfo::class.java)
                } else {
                    type = typeNodeToType(typeLookup, cu.source, "variable '${n.name.text}'", n.name, n.type)
                    if (n.initializer != null) {
                        val initializerType = n.initializer.getAnnotation(TypeInfo::class.java)
                        if (!assignable(type, initializerType)) throw CompilerException(cu.source, "The initializer expression of type '${typeToString(initializerType)}' can not be assigned left the variable '${n.name.text}' of type '${typeToString(type)}'", n.name)
                    }
                }
                variableLookup.add(Variable(n.name, n.name.text, VariableType.Local, type))
            }

            override fun returnStatement(n: ReturnNode) {
                if (currentFunction == null) throw CompilerException(cu.source, "Found return statement outside of function", n.firstToken)
                if (n.expression != null) {
                    val exprType = n.expression.getAnnotation(TypeInfo::class.java)
                    if (!assignable(currentFunction!!.returnType, exprType)) throw CompilerException(cu.source, "Can not return value of type '${typeToString(exprType)}' from function returning type '${typeToString(currentFunction!!.returnType)}'", n.expression.firstToken)
                    n.setAnnotation(currentFunction!!.returnType, TypeInfo::class.java)
                } else {
                    if (currentFunction!!.returnType != VoidType) throw CompilerException(cu.source, "Expected a value of type '${typeToString(currentFunction!!.returnType)} to be returned from function ${currentFunction!!.name}", n.firstToken)
                }
            }

            override fun ifStatement(n: IfNode) {
                val conditionType = n.condition.getAnnotation(TypeInfo::class.java)
                if (conditionType != BooleanType) throw CompilerException(cu.source, "Condition of 'if' statement must have a boolean type", n.condition.firstToken)
            }

            override fun forStatement(n: ForNode) {
                val conditionType = n.condition.getAnnotation(TypeInfo::class.java)
                if (conditionType != BooleanType) throw CompilerException(cu.source, "Condition of 'for' statement must have a boolean type", n.condition.firstToken)
            }

            override fun whileStatement(n: WhileNode) {
                val conditionType = n.condition.getAnnotation(TypeInfo::class.java)
                if (conditionType != BooleanType) throw CompilerException(cu.source, "Condition of 'while' statement must have a boolean type", n.condition.firstToken)
            }

            override fun doStatement(n: DoNode) {
                val conditionType = n.condition.getAnnotation(TypeInfo::class.java)
                if (conditionType != BooleanType) throw CompilerException(cu.source, "Condition of if statement must have a boolean type", n.condition.firstToken)
            }

            override fun breakStatement(n: BreakNode) {
                if (loopStack.size == 0) throw CompilerException(cu.source, "Break statement found outside of for, while or do loop", n.firstToken)
                n.setAnnotation(loopStack.last(), AstNode::class.java)
            }

            override fun continueStatement(n: ContinueNode) {
                if (loopStack.size == 0) throw CompilerException(cu.source, "Continue statement found outside of for, while or do loop", n.firstToken)
                n.setAnnotation(loopStack.last(), AstNode::class.java)
            }

            override fun unaryOperator(n: UnaryOperatorNode) {
                when (n.opType.type) {
                    TokenType.PLUS, TokenType.MINUS -> {
                        val exprType = n.expr.getAnnotation(TypeInfo::class.java)
                        if (exprType !is PrimitiveType) throw CompilerException(cu.source, "Unary '${n.opType.text}' can only be applied left numeric types", n.opType)
                        if (!exprType.isNumeric) throw CompilerException(cu.source, "Unary operator '${n.opType.text}' requires a numeric type", n.opType)
                        n.setAnnotation(exprType, TypeInfo::class.java)
                    }
                    TokenType.NOT -> {
                        val exprType = n.expr.getAnnotation(TypeInfo::class.java)
                        if (exprType !is PrimitiveType) throw  CompilerException(cu.source, "Unary operator '${n.opType.text}' requires a numeric or boolean type", n.opType)
                        if (!exprType.isNumeric or (exprType != BooleanType)) throw CompilerException(cu.source, "Unary operator '${n.opType.text}' requires a numeric or boolean type", n.opType)
                        n.setAnnotation(exprType, TypeInfo::class.java)
                    }
                    else -> throw CompilerException(cu.source, "Unknown unary operator ${n.opType.text}", n.opType)
                }
            }

            override fun binaryOperator(n: BinaryOperatorNode) {
                val leftType = n.left.getAnnotation(TypeInfo::class.java)
                val rightType = n.right.getAnnotation(TypeInfo::class.java)

                when (n.opType.type) {
                    TokenType.EQUAL -> {
                        if ((n.left !is VariableAccessNode) and (n.left !is ArrayAccessNode) and (n.left !is FieldAccessNode)) throw CompilerException(cu.source, "Left-hand side of assignment is not a variable, field or array element", n.left.firstToken)
                        if ((leftType == NullType) and (rightType !is OptionalType)) throw CompilerException(cu.source, "Can not assign null to non-nullable type", n.left.firstToken)
                        if (!assignable(leftType, rightType)) throw CompilerException(cu.source, "Can not assign a '${typeToString(rightType)}' left a '${typeToString(leftType)}", n.left.firstToken)
                        n.setAnnotation(leftType, TypeInfo::class.java)
                    }
                    TokenType.PLUS_EQUAL, TokenType.MINUS_EQUAL, TokenType.MUL_EQUAL, TokenType.DIV_EQUAL, TokenType.MOD_EQUAL -> {
                        if ((n.left !is VariableAccessNode) and (n.left !is ArrayAccessNode) and (n.left !is FieldAccessNode)) throw CompilerException(cu.source, "Left-hand side of assignment is not a variable, field or array element", n.left.firstToken)
                        if ((leftType !is PrimitiveType) and (rightType !is PrimitiveType)) throw CompilerException(cu.source, "Operator '${n.opType.text}' requires numeric types on the left- and right-hand side", n.opType)
                        val leftPrimType = leftType as PrimitiveType
                        val rightPrimType = rightType as PrimitiveType

                        if (!leftPrimType.isNumeric and !rightPrimType.isNumeric) throw CompilerException(cu.source, "Operator '${n.opType.text}' requires numeric types on the left- and right-hand side", n.opType)
                        if (!assignable(leftType, rightType)) throw CompilerException(cu.source, "Can not '${n.opType.text}' value of type '${typeToString(leftType)}' with a value of type '${typeToString(rightType)}", n.left.firstToken)
                        n.setAnnotation(leftType, TypeInfo::class.java)
                    }
                    TokenType.OR, TokenType.AND, TokenType.XOR -> {
                        if ((leftType !is PrimitiveType) and (rightType !is PrimitiveType)) throw CompilerException(cu.source, "Operator '${n.opType.text}' requires a numeric type on the left- and right-hand side", n.opType)
                        val leftPrimType = leftType as PrimitiveType
                        val rightPrimType = rightType as PrimitiveType

                        if (leftPrimType != BooleanType && leftPrimType != Int8Type && leftPrimType != Int16Type && leftPrimType != Int32Type && leftPrimType != Int64Type) throw CompilerException(cu.source, "Operator '${n.opType.text} requires an integer or boolean type on the left-hand side", n.left.firstToken)
                        if (rightPrimType != BooleanType && rightPrimType != Int8Type && rightPrimType != Int16Type && rightPrimType != Int32Type && rightPrimType != Int64Type) throw CompilerException(cu.source, "Operator '${n.opType.text} requires an integer or boolean type on the right-hand side", n.left.firstToken)
                        if (leftPrimType != rightPrimType) throw CompilerException(cu.source, "Can not '${n.opType.text}' value of type '${typeToString(leftType)}' with a value of type '${typeToString(rightType)}", n.left.firstToken)
                        n.setAnnotation(leftType, TypeInfo::class.java)
                    }
                    TokenType.DOUBLE_EQUAL, TokenType.NOT_EQUAL -> {
                        if (!comparable(leftType, rightType)) throw CompilerException(cu.source, "Can not '${n.opType.text}' value of type '${typeToString(leftType)}' with a value of type '${typeToString(rightType)}", n.left.firstToken)
                        n.setAnnotation(BooleanType, TypeInfo::class.java)
                    }
                    TokenType.TRIPLE_EQUAL -> {
                        if (leftType is PrimitiveType || rightType is PrimitiveType) throw CompilerException(cu.source, "Operator '${n.opType.text}' requires a structure or array type on either side", n.left.firstToken)
                        n.setAnnotation(BooleanType, TypeInfo::class.java)

                    }
                    TokenType.LESS, TokenType.LESSEQUAL, TokenType.GREATER, TokenType.GREATEREQUAL -> {
                        if (leftType !is PrimitiveType || rightType !is PrimitiveType) throw CompilerException(cu.source, "Can not '${n.opType.text}' value of type '${typeToString(leftType)}' with a value of type '${typeToString(rightType)}", n.left.firstToken)
                        if (leftType.isNumeric || rightType.isNumeric) throw CompilerException(cu.source, "Operator '${n.opType.text}' requires a numeric types on either side", n.left.firstToken)
                        if (!comparable(leftType, rightType)) throw CompilerException(cu.source, "Can not '${n.opType.text}' value of type '${typeToString(leftType)}' with a value of type '${typeToString(rightType)}", n.left.firstToken)
                        n.setAnnotation(BooleanType, TypeInfo::class.java)
                    }
                    TokenType.SHL, TokenType.SHR -> {
                        if ((leftType !is PrimitiveType) and (rightType !is PrimitiveType)) throw CompilerException(cu.source, "Operator '${n.opType.text}' requires an integer type on the left- and right-hand side", n.opType)
                        val leftPrimType = leftType as PrimitiveType
                        val rightPrimType = rightType as PrimitiveType

                        if (leftPrimType != Int8Type && leftPrimType != Int16Type && leftPrimType != Int32Type && leftPrimType != Int64Type) throw CompilerException(cu.source, "Operator '${n.opType.text} requires an integer type on the left-hand side", n.left.firstToken)
                        if (rightPrimType != Int8Type && rightPrimType != Int16Type && rightPrimType != Int32Type && rightPrimType != Int64Type) throw CompilerException(cu.source, "Operator '${n.opType.text} requires an integer type on the right-hand side", n.left.firstToken)
                        n.setAnnotation(leftType, TypeInfo::class.java)
                    }
                    TokenType.PLUS, TokenType.MINUS -> {
                        if ((leftType !is PrimitiveType) and (rightType !is PrimitiveType)) throw CompilerException(cu.source, "Operator '${n.opType.text}' requires an integer type on the left- and right-hand side", n.opType)
                        val leftPrimType = leftType as PrimitiveType
                        val rightPrimType = rightType as PrimitiveType

                        if (leftPrimType != FloatType && leftPrimType != DoubleType && leftPrimType != Int8Type && leftPrimType != Int16Type && leftPrimType != Int32Type && leftPrimType != Int64Type) throw CompilerException(cu.source, "Operator '${n.opType.text} requires an integer type on the left-hand side", n.left.firstToken)
                        if (rightPrimType != FloatType && rightPrimType != DoubleType && rightPrimType != Int8Type && rightPrimType != Int16Type && rightPrimType != Int32Type && rightPrimType != Int64Type) throw CompilerException(cu.source, "Operator '${n.opType.text} requires an integer type on the right-hand side", n.left.firstToken)
                        n.setAnnotation(leftType, TypeInfo::class.java)
                    }
                    TokenType.DIV, TokenType.MUL, TokenType.MODULO -> {
                        if ((leftType !is PrimitiveType) and (rightType !is PrimitiveType)) throw CompilerException(cu.source, "Operator '${n.opType.text}' requires an integer type on the left- and right-hand side", n.opType)
                        val leftPrimType = leftType as PrimitiveType
                        val rightPrimType = rightType as PrimitiveType

                        if (leftPrimType != FloatType && leftPrimType != DoubleType && leftPrimType != Int8Type && leftPrimType != Int16Type && leftPrimType != Int32Type && leftPrimType != Int64Type) throw CompilerException(cu.source, "Operator '${n.opType.text} requires an integer type on the left-hand side", n.left.firstToken)
                        if (rightPrimType != FloatType && rightPrimType != DoubleType && rightPrimType != Int8Type && rightPrimType != Int16Type && rightPrimType != Int32Type && rightPrimType != Int64Type) throw CompilerException(cu.source, "Operator '${n.opType.text} requires an integer type on the right-hand side", n.left.firstToken)
                        n.setAnnotation(leftType, TypeInfo::class.java)
                    }
                    else -> throw CompilerException(cu.source, "Unknown binary operator ${n.opType.text}", n.opType)
                }
            }

            override fun ternaryOperator(n: TernaryOperatorNode) {
                // FIXME
                throw UnsupportedOperationException("Type check for ternary operator not implemented")
            }

            override fun charLiteral(n: CharacterLiteralNode) {
                // FIXME
                throw UnsupportedOperationException("Type check for char literal not implemented")
            }

            override fun stringLiteral(n: StringLiteralNode) {
                // FIXME
                throw UnsupportedOperationException("Type check for string literal not implemented")
            }

            override fun numberLiteral(n: NumberLiteralNode) {
                if (n.literal.text.contains(".") or n.literal.text.contains("d")) {
                    if (n.literal.text.contains("d"))
                        n.setAnnotation(DoubleType, TypeInfo::class.java)
                    else
                        n.setAnnotation(FloatType, TypeInfo::class.java)
                } else {
                    if (n.literal.text.contains("l"))
                        n.setAnnotation(Int64Type, TypeInfo::class.java)
                    else
                        n.setAnnotation(Int32Type, TypeInfo::class.java)
                }
            }

            override fun booleanLiteral(n: BooleanLiteralNode) {
                n.setAnnotation(BooleanType, TypeInfo::class.java)
            }

            override fun nullLiteral(n: NullLiteralNode) {
                n.setAnnotation(NullType, TypeInfo::class.java)
            }

            override fun variableAccess(n: VariableAccessNode) {
                val variable = variableLookup.lookup(n.varName.text)
                if (variable == null) throw CompilerException(cu.source, "Variable '${n.varName.text}' is not defined", n.varName)
                n.setAnnotation(variable.type, TypeInfo::class.java)
            }

            override fun arrayAccess(n: ArrayAccessNode) {
                val indexType = n.index.getAnnotation(TypeInfo::class.java)
                val arrayType = n.array.getAnnotation(TypeInfo::class.java)
                if (indexType !is PrimitiveType) throw CompilerException(cu.source, "Index expression must be an integer value", n.index.firstToken)
                if (arrayType !is ArrayType) throw CompilerException(cu.source, "Indexing operator requires array type, found ${typeToString(arrayType)}", n.array.firstToken)
                n.setAnnotation(arrayType.elementType, TypeInfo::class.java)
            }

            override fun fieldAccess(n: FieldAccessNode) {
                val baseType = n.base.getAnnotation(TypeInfo::class.java)
                if (baseType !is StructureType) throw CompilerException(cu.source, "Can't access field '${n.varName.text}' of non-structure type '$baseType'", n.varName)
                for ((name, type) in baseType.fields) {
                    if (name.equals(n.varName.text)) {
                        n.setAnnotation(type, TypeInfo::class.java)
                        return
                    }
                }
                // FIXME resolve fully qualified names as a chain of VariableAccessNode -> FieldAccessNode -> FieldAccessNode
                throw CompilerException(cu.source, "Type '${baseType.fullyQualifiedName()}' has no field '${n.varName.text}'", n.varName)
            }

            override fun functionCall(n: FunctionCallNode) {
                // FIXME
            }

            override fun parenthesis(n: ParenthesisNode) {
                n.setAnnotation(n.expr.getAnnotation(TypeInfo::class.java), TypeInfo::class.java)
            }
        })
    }
}

fun typeToString(t: TypeInfo): String {
    when (t) {
        is PrimitiveType -> return t.name
        is StructureType -> return if (t.module.isEmpty()) t.name else t.module + "." + t.name
        is FunctionType -> {
            val buffer = StringBuffer()
            if (!t.module.isEmpty()) {
                buffer.append(t.module)
                buffer.append(".")
            }
            buffer.append(t.name)
            buffer.append("(")
            for (i in t.parameters.indices) {
                buffer.append(typeToString(t.parameters[i].type))
                if (i != t.parameters.lastIndex) buffer.append(", ")
            }
            buffer.append(")")
            return buffer.toString()
        }
        is ArrayType -> return typeToString(t.elementType) + "[]"
        is OptionalType -> return typeToString(t.elementType) + "?"
        else -> throw RuntimeException("Unknown TypeInfo subtype $t")
    }
}

fun assignable(to: TypeInfo, from: TypeInfo): Boolean {
    if (to.javaClass != from.javaClass) return false

    if ((to is PrimitiveType) and (from is PrimitiveType)) {
        if (to == from) return true
    } else if ((to is StructureType) and (from is StructureType)) {
        if (to == from) return true
    } else if ((to is ArrayType) and (from is ArrayType)) {
        val toArray = to as ArrayType
        val fromArray = from as ArrayType
        return assignable(toArray.elementType, fromArray.elementType)
    }
    return false
}

fun comparable(left: TypeInfo, right: TypeInfo): Boolean {
    if (left.javaClass != right.javaClass) return false

    if ((left is PrimitiveType) and (right is PrimitiveType)) {
        if (left == right) return true
    } else if ((left is StructureType) and (right is StructureType)) {
        if (left == right) return true
    } else if ((left is ArrayType) and (right is ArrayType)) {
        val toArray = left as ArrayType
        val fromArray = right as ArrayType
        return comparable(toArray.elementType, fromArray.elementType)
    }
    return false
}
