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

fun checkTypes(state: CompilerState) {
    gatherTopLevelTypes(state)
    resolveTypes(state)
}

/**
 * Resolves the types referenced by structures and functions.
 * Does not resolve types referenced in function bodies. Assumes
 * FunctionNodes and StructureNodes have the StructureType
 * and FunctionType annotation set. Adds implicit "this"
 * parameter to constructors and methods. Adds default constructor
 * if no other constructor is given.
 */
private fun gatherTopLevelTypes(state: CompilerState) {
    for (cu in state.compilationUnits) {
        val module = cu.getAnnotation(Module::class.java)

        val importedModules = mutableListOf<Module>()
        importedModules.add(state.modules[""]!!)
        importedModules.add(module)
        for (i in cu.imports) {
            if (!state.modules.containsKey(i.importName)) throw CompilerException(cu.source, "Could not find imported module '${i.importName}'", i.firstToken.line, i.firstToken.column, i.firstToken.column + i.importName.length)
            importedModules.add(state.modules[i.importName]!!)
        }
        val lookup = ModuleTypeLookup(importedModules)

        traverseAst(cu, object : AstVisitorAdapter() {
            override fun structure(n: StructureNode) {
                val structure = n.getAnnotation(StructureType::class.java)

                // resolve types of fields
                for (field in n.fields) {
                    // fields must have a type given, infering the type right the initializer is to hard at the moment
                    if (field.type is NoTypeNode) {
                        throw CompilerException(cu.source, "No type given for field '${field.name.text}'", field.name)
                    } else {
                        structure.fields.add(NamedType(field.name.text, typeNodeToType(lookup, cu.source, "field '${field.name.text}'", field.firstToken, field.type)))
                    }
                }

                // resolve types of methods, add "this" as a parameter, and add them to the module
                var hasConstructor = false
                for (f in n.functions) {
                    val scope = if (f.name.text.equals("constructor")) {
                        hasConstructor = true
                        FunctionUsage.Constructor
                    } else {
                        FunctionUsage.Method
                    }

                    val func = FunctionType(Location(cu.source, f.name.line, f.name.column), module.name, f.name.text, f.extern, usage = scope)
                    f.setAnnotation(func, FunctionType::class.java)
                    function(f)
                    func.parameters.add(0, NamedType("this", structure))
                    if (func.usage == FunctionUsage.Constructor) func.returnType = structure

                    structure.functions.add(func)
                    if (func.usage == FunctionUsage.Constructor) module.addFunction(structure.name, func)
                }

                if (!hasConstructor) {
                    val defaultConstructor = FunctionType(Location(cu.source, n.name.line, n.name.column), module.name, "constructor", false, usage = FunctionUsage.Constructor)
                    defaultConstructor.parameters.add(0, NamedType("this", structure))
                    defaultConstructor.returnType = structure

                    structure.functions.add(defaultConstructor)
                    module.addFunction(structure.name, defaultConstructor)
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
private fun resolveTypes(state: CompilerState) {
    for (cu in state.compilationUnits) {
        val module = cu.getAnnotation(Module::class.java)

        val importedModules = mutableListOf<Module>()
        importedModules.add(state.modules[""]!!)
        importedModules.add(module)
        for (i in cu.imports) {
            if (!state.modules.containsKey(i.importName)) throw CompilerException(cu.source, "Could not find imported module '${i.importName}'", i.firstToken.line, i.firstToken.column, i.firstToken.column + i.importName.length)
            importedModules.add(state.modules[i.importName]!!)
        }
        val typeLookup = ModuleTypeLookup(importedModules)
        val variableLookup = VariableLookup()

        traverseAst(cu, object : AstVisitorAdapter() {
            var currentStructure: StructureType? = null
            var currentFunction: FunctionType? = null
            val loopStack = mutableListOf<AstNode>()

            override fun pushScope(n: AstNode) {
                variableLookup.pushScope()
                when (n) {
                    is FunctionNode -> {
                        currentFunction = n.getAnnotation(FunctionType::class.java)
                        val func = currentFunction
                        if (func == null) throw CompilerException(cu.source, "Internal: No type for function node", n.firstToken)

                        // Add implicit this parameter
                        if (func.usage == FunctionUsage.Constructor || func.usage == FunctionUsage.Method) {
                            val struct = currentStructure
                            if (struct == null) throw CompilerException(cu.source, "Internal: No current structure set", n.firstToken)
                            variableLookup.add(Variable(n.name, "this", VariableType.Parameter, struct))
                        }

                        for (p in n.parameters) {
                            val pt = p.getAnnotation(TypeInfo::class.java)
                            variableLookup.add(Variable(p.name, p.name.text, VariableType.Parameter, pt))
                        }
                    }
                    is StructureNode -> {
                        currentStructure = n.getAnnotation(StructureType::class.java)
                    }
                }
            }

            override fun popScope(n: AstNode) {
                if (n is FunctionNode) currentFunction = null
                if (n is StructureNode) currentStructure = null
                variableLookup.popScope()
            }

            override fun pushLoop(n: AstNode) {
                loopStack.add(n)
            }

            override fun popLoop(n: AstNode) {
                loopStack.removeAt(loopStack.lastIndex)
            }

            override fun structure(n: StructureNode) {
                // add the structure functions to a dummy module so they
                // can be resolved without specifying this.functionName
                val structModule = Module(module.name)
                val structure = n.getAnnotation(StructureType::class.java)
                for (f in structure.functions)
                    if (f.usage != FunctionUsage.Constructor)
                        structModule.addFunction(f.name, f)
                importedModules.add(structModule)
                for (f in n.functions) traverseAst(f, this)
                importedModules.removeAt(importedModules.lastIndex)
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
                        // FIXME add widening/casts if necessary
                    }
                }
                variableLookup.add(Variable(n.name, n.name.text, VariableType.Local, type))
            }

            override fun returnStatement(n: ReturnNode) {
                if (currentFunction == null) throw CompilerException(cu.source, "Found return statement outside of function", n.firstToken)
                if (n.expression != null) {
                    val exprType = n.expression.getAnnotation(TypeInfo::class.java)
                    if (!assignable(currentFunction!!.returnType, exprType)) throw CompilerException(cu.source, "Can not return value of type '${typeToString(exprType)}' from function returning type '${typeToString(currentFunction!!.returnType)}'", n.expression.firstToken)
                    // FIXME add widening/casts if necessary
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
                        if (!exprType.isNumeric || (exprType != BooleanType)) throw CompilerException(cu.source, "Unary operator '${n.opType.text}' requires a numeric or boolean type", n.opType)
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
                        if (!assignable(leftType, rightType)) throw CompilerException(cu.source, "Can not assign a '${typeToString(rightType)}' to a '${typeToString(leftType)}", n.left.firstToken)
                        // FIXME add widening/casts if necessary
                        n.setAnnotation(leftType, TypeInfo::class.java)
                    }
                    TokenType.PLUS_EQUAL, TokenType.MINUS_EQUAL, TokenType.MUL_EQUAL, TokenType.DIV_EQUAL, TokenType.MOD_EQUAL -> {
                        if ((n.left !is VariableAccessNode) and (n.left !is ArrayAccessNode) and (n.left !is FieldAccessNode)) throw CompilerException(cu.source, "Left-hand side of assignment is not a variable, field or array element", n.left.firstToken)
                        if ((leftType !is PrimitiveType) and (rightType !is PrimitiveType)) throw CompilerException(cu.source, "Operator '${n.opType.text}' requires numeric types on the left- and right-hand side", n.opType)
                        val leftPrimType = leftType as PrimitiveType
                        val rightPrimType = rightType as PrimitiveType

                        if (!leftPrimType.isNumeric and !rightPrimType.isNumeric) throw CompilerException(cu.source, "Operator '${n.opType.text}' requires numeric types on the left- and right-hand side", n.opType)
                        if (!assignable(leftType, rightType)) throw CompilerException(cu.source, "Can not '${n.opType.text}' value of type '${typeToString(leftType)}' with a value of type '${typeToString(rightType)}", n.left.firstToken)
                        // FIXME add widening/casts if necessary
                        n.setAnnotation(leftType, TypeInfo::class.java)
                    }
                    TokenType.OR, TokenType.AND, TokenType.XOR -> {
                        if ((leftType !is PrimitiveType) and (rightType !is PrimitiveType)) throw CompilerException(cu.source, "Operator '${n.opType.text}' requires a numeric type on the left- and right-hand side", n.opType)
                        val leftPrimType = leftType as PrimitiveType
                        val rightPrimType = rightType as PrimitiveType

                        if (leftPrimType != BooleanType && leftPrimType != Int8Type && leftPrimType != Int16Type && leftPrimType != Int32Type && leftPrimType != Int64Type) throw CompilerException(cu.source, "Operator '${n.opType.text} requires an integer or boolean type on the left-hand side", n.left.firstToken)
                        if (rightPrimType != BooleanType && rightPrimType != Int8Type && rightPrimType != Int16Type && rightPrimType != Int32Type && rightPrimType != Int64Type) throw CompilerException(cu.source, "Operator '${n.opType.text} requires an integer or boolean type on the right-hand side", n.left.firstToken)
                        // FIXME add widening/casts if necessary
                        if (leftPrimType != rightPrimType) throw CompilerException(cu.source, "Can not '${n.opType.text}' value of type '${typeToString(leftType)}' with a value of type '${typeToString(rightType)}", n.left.firstToken)
                        n.setAnnotation(leftType, TypeInfo::class.java)
                    }
                    TokenType.DOUBLE_EQUAL, TokenType.NOT_EQUAL -> {
                        if (!comparable(leftType, rightType)) throw CompilerException(cu.source, "Can not '${n.opType.text}' value of type '${typeToString(leftType)}' with a value of type '${typeToString(rightType)}", n.left.firstToken)
                        // FIXME add widening/casts if necessary
                        n.setAnnotation(BooleanType, TypeInfo::class.java)
                    }
                    TokenType.TRIPLE_EQUAL -> {
                        if (leftType is PrimitiveType || rightType is PrimitiveType) throw CompilerException(cu.source, "Operator '${n.opType.text}' requires a structure or array type on either side", n.left.firstToken)
                        // FIXME add widening/casts if necessary
                        n.setAnnotation(BooleanType, TypeInfo::class.java)

                    }
                    TokenType.LESS, TokenType.LESSEQUAL, TokenType.GREATER, TokenType.GREATEREQUAL -> {
                        if (leftType !is PrimitiveType || rightType !is PrimitiveType) throw CompilerException(cu.source, "Can not '${n.opType.text}' value of type '${typeToString(leftType)}' with a value of type '${typeToString(rightType)}", n.left.firstToken)
                        if (leftType.isNumeric || rightType.isNumeric) throw CompilerException(cu.source, "Operator '${n.opType.text}' requires a numeric types on either side", n.left.firstToken)
                        if (!comparable(leftType, rightType)) throw CompilerException(cu.source, "Can not '${n.opType.text}' value of type '${typeToString(leftType)}' with a value of type '${typeToString(rightType)}", n.left.firstToken)
                        // FIXME add widening/casts if necessary
                        n.setAnnotation(BooleanType, TypeInfo::class.java)
                    }
                    TokenType.SHL, TokenType.SHR -> {
                        if ((leftType !is PrimitiveType) and (rightType !is PrimitiveType)) throw CompilerException(cu.source, "Operator '${n.opType.text}' requires an integer type on the left- and right-hand side", n.opType)
                        val leftPrimType = leftType as PrimitiveType
                        val rightPrimType = rightType as PrimitiveType

                        if (leftPrimType != Int8Type && leftPrimType != Int16Type && leftPrimType != Int32Type && leftPrimType != Int64Type) throw CompilerException(cu.source, "Operator '${n.opType.text} requires an integer type on the left-hand side", n.left.firstToken)
                        if (rightPrimType != Int8Type && rightPrimType != Int16Type && rightPrimType != Int32Type && rightPrimType != Int64Type) throw CompilerException(cu.source, "Operator '${n.opType.text} requires an integer type on the right-hand side", n.left.firstToken)
                        // FIXME add widening/casts if necessary
                        n.setAnnotation(leftType, TypeInfo::class.java)
                    }
                    TokenType.PLUS, TokenType.MINUS -> {
                        if ((leftType !is PrimitiveType) and (rightType !is PrimitiveType)) throw CompilerException(cu.source, "Operator '${n.opType.text}' requires an integer type on the left- and right-hand side", n.opType)
                        val leftPrimType = leftType as PrimitiveType
                        val rightPrimType = rightType as PrimitiveType

                        if (leftPrimType != FloatType && leftPrimType != DoubleType && leftPrimType != Int8Type && leftPrimType != Int16Type && leftPrimType != Int32Type && leftPrimType != Int64Type) throw CompilerException(cu.source, "Operator '${n.opType.text} requires an integer type on the left-hand side", n.left.firstToken)
                        if (rightPrimType != FloatType && rightPrimType != DoubleType && rightPrimType != Int8Type && rightPrimType != Int16Type && rightPrimType != Int32Type && rightPrimType != Int64Type) throw CompilerException(cu.source, "Operator '${n.opType.text} requires an integer type on the right-hand side", n.left.firstToken)
                        // FIXME add widening/casts if necessary
                        n.setAnnotation(leftType, TypeInfo::class.java)
                    }
                    TokenType.DIV, TokenType.MUL, TokenType.MODULO -> {
                        if ((leftType !is PrimitiveType) and (rightType !is PrimitiveType)) throw CompilerException(cu.source, "Operator '${n.opType.text}' requires an integer type on the left- and right-hand side", n.opType)
                        val leftPrimType = leftType as PrimitiveType
                        val rightPrimType = rightType as PrimitiveType

                        if (leftPrimType != FloatType && leftPrimType != DoubleType && leftPrimType != Int8Type && leftPrimType != Int16Type && leftPrimType != Int32Type && leftPrimType != Int64Type) throw CompilerException(cu.source, "Operator '${n.opType.text} requires an integer type on the left-hand side", n.left.firstToken)
                        if (rightPrimType != FloatType && rightPrimType != DoubleType && rightPrimType != Int8Type && rightPrimType != Int16Type && rightPrimType != Int32Type && rightPrimType != Int64Type) throw CompilerException(cu.source, "Operator '${n.opType.text} requires an integer type on the right-hand side", n.left.firstToken)
                        // FIXME add widening/casts if necessary
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
                if (n.literal.text.contains(".") || n.literal.text.contains("d")) {
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
                val root = n.function

                // FIXME add struct methods, add first class function support (call from field/variable, closures?)
                when (root) {
                // simplest case, variable access, e.g. sqrt(1234), just look it up in the type lookup
                    is VariableAccessNode -> {
                        val functionName = root.varName
                        val funcs = typeLookup.lookupFunction(functionName.text)
                        if (funcs.size == 0) {
                            // FIXME reference to a function instance
                            throw CompilerException(cu.source, "First class functions not implemented", n.firstToken)
                        } else {
                            resolveFunction(cu.source, funcs, n, functionName)
                        }
                    }

                // case 2, field access, either fully qualified name via VariableAccess -> FieldAccess chain, e.g. kek.math.sqrt(1234)
                // or method call
                    is FieldAccessNode -> {
                        val functionName = tryMatchFullyQualifiedName(root)
                        val funcs = typeLookup.lookupFunction(functionName)
                        if (funcs.size == 0) {
                            traverseAst(root.base, this)
                            val baseType = root.base.getAnnotation(TypeInfo::class.java)
                            if (baseType is StructureType) {
                                val candidates = mutableListOf<FunctionType>()
                                for (f in baseType.functions) {
                                    if (f.usage == FunctionUsage.Method && f.name.equals(root.varName.text))
                                        candidates.add(f)
                                }
                                resolveFunction(cu.source, candidates, n, root.varName)
                            } else {
                                // FIXME reference to a function instance
                                throw CompilerException(cu.source, "First class functions not implemented", n.firstToken)
                            }
                        } else {
                            resolveFunction(cu.source, funcs, n, root.varName)
                        }
                    }

                // FIXME reference to a function instance
                    else -> {
                        throw CompilerException(cu.source, "First class functions not implemented", n.function.firstToken)
                    }
                }
            }

            override fun parenthesis(n: ParenthesisNode) {
                n.setAnnotation(n.expr.getAnnotation(TypeInfo::class.java), TypeInfo::class.java)
            }
        })
    }
}

fun resolveFunction(source: Source, funcs: List<FunctionType>, call: FunctionCallNode, functionName: Token) {
    val candidates = mutableListOf<FunctionType>()
    for (f in funcs) {
        if (checkFunctionParameters(source, f, call, false))
            candidates.add(f)
    }
    if (candidates.size == 1) {
        checkFunctionParameters(source, candidates[0], call, true)
        call.setAnnotation(candidates[0], FunctionType::class.java)
        call.setAnnotation(candidates[0].returnType, TypeInfo::class.java)
        return
    } else {
        val types = StringBuffer()
        types.append("(")
        for (i in call.arguments.indices) {
            types.append(typeToString(call.arguments[i].getAnnotation(TypeInfo::class.java)))
            if (i < call.arguments.lastIndex) types.append(", ")
        }
        types.append(")")

        // FIXME show candidates
        if (candidates.size == 0) throw CompilerException(source, "Could not find function '${functionName.text}' matching parameter types ${types.toString()}", functionName)
        else throw CompilerException(source, "Found multiple functions '${functionName.text}' matching parameter types ${types.toString()}", functionName)
    }
}

fun checkFunctionParameters(source: Source, candidate: FunctionType, callNode: FunctionCallNode, throwOnError: Boolean = true): Boolean {
    val candidateParams = candidate.parameters
    val args = callNode.arguments

    // leave out implicit first parameter if this is a method or constructor
    val comparisonStartIndex: Int
    var candidateParamsCount: Int
    if (candidate.usage == FunctionUsage.Method || candidate.usage == FunctionUsage.Constructor) {
        candidateParamsCount = candidate.parameters.size - 1
        comparisonStartIndex = 1
    } else {
        candidateParamsCount = candidate.parameters.size
        comparisonStartIndex = 0
    }
    if (candidateParamsCount != args.size) {
        if (throwOnError)
            throw CompilerException(source, "Wrong number of arguments, expected ${candidate.parameters.size}, got ${args.size}", callNode.firstToken)
        else
            return false
    }

    var j = 0
    for (i in comparisonStartIndex..candidate.parameters.size - 1) {
        val candidateParamType = candidateParams[i].type
        val argType = args[j].getAnnotation(TypeInfo::class.java)

        // FIXME add widening conversions or casts
        if (!assignable(candidateParamType, argType)) {
            if (throwOnError)
                throw CompilerException(source, "Can not pass argument of type ${typeToString(argType)} for parameter '${candidateParams[i].name}' of type ${typeToString(candidateParamType)}", args[j].firstToken)
            else
                return false
        }
        j++
    }
    return true
}

fun assignable(to: TypeInfo, from: TypeInfo): Boolean {
    if (to.javaClass != from.javaClass) return false

    // FIXME take into account possible widening/casts
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

    // FIXME take into account possible widening/casts
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

fun tryMatchFullyQualifiedName(field: FieldAccessNode): String {
    val parts = mutableListOf<String>()
    var node: AstNode = field

    while (true) {
        if (node is FieldAccessNode) {
            parts.add(node.varName.text)
            node = node.base
        } else if (node is VariableAccessNode) {
            parts.add(node.varName.text)
            break
        } else {
            return ""
        }
    }

    parts.reverse()
    val buffer = StringBuffer()
    for (i in 0..parts.size - 1) {
        buffer.append(parts[i])
        if (i < parts.size - 1) buffer.append(".")
    }
    return buffer.toString()
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
