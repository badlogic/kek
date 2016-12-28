package kek.compiler

import kek.runtime.*
import javax.lang.model.type.NoType

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

class Variable(val location: Token, val name: String, val symbolType: VariableType, val type: TypeInfo)

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
        val lines = source.sourceCode.split('\n');
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
    defaultModule.primitiveTypes.put(IntType.name, IntType)
    defaultModule.primitiveTypes.put(Int8Type.name, Int8Type)
    defaultModule.primitiveTypes.put(Int16Type.name, Int16Type)
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
                    // fields must have a type given, infering the type from the initializer is to hard at the moment
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
                    parameter.setAnnotation(type, NamedType::class.java)
                }

                // resolve type of return value
                if (n.returnType is NoTypeNode) function.returnType = VoidType
                else function.returnType = typeNodeToType(lookup, cu.source, "return value", n.name, n.returnType)
            }
        }, setOf<Class<out AstNode>>(CompilationUnitNode::class.java, StructureNode::class.java, FunctionNode::class.java));
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
        throw CompilerException(source, "Multiple types matching type of ${typeOfWhat}", token)
    } else {
        throw CompilerException(source, "Could not find type '${node.fullyQualfiedName()}' of ${typeOfWhat}", token)
    }
}

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

            override fun pushScope(n: AstNode) {
                variableLookup.pushScope()
                when (n) {
                    is FunctionNode -> {
                        for (p in n.parameters) {
                            val pt = p.getAnnotation(NamedType::class.java)
                            variableLookup.add(Variable(p.name, pt.name, VariableType.Parameter, pt))
                        }
                    }
                }
            }

            override fun popScope(n: AstNode) {
                variableLookup.popScope()
            }

            override fun variableDeclaration(n: VariableDeclarationNode) {
                var variable = variableLookup.lookup(n.name.text);
                if (variable != null) throw CompilerException(cu.source, "Variable '${n.name.text}' already defined at (${variable.location.line}:${variable.location.column})", n.name)
                val type: TypeInfo
                if (n.type is NoTypeNode) {
                    if (n.initializer == null) throw CompilerException(cu.source, "Variable '${n.name.text}' needs a type or initializer", n.name)
                    type = n.initializer.getAnnotation(TypeInfo::class.java)
                } else {
                    type = typeNodeToType(typeLookup, cu.source, "variable '${n.name.text}'", n.name, n.type)
                    if (n.initializer != null) {
                        val initializerType = n.initializer.getAnnotation(TypeInfo::class.java)
                        if (!typesEqual(type, initializerType)) throw CompilerException(cu.source, "The variable '${n.name.text} of type ${typeToString(type)}", n.name)
                    }
                }
                variableLookup.add(Variable(n.name, n.name.text, VariableType.Local, type))
            }

            override fun returnStatement(n: ReturnNode) {
                // FIXME
            }

            override fun ifStatement(n: IfNode) {
                // FIXME
            }

            override fun forStatement(n: ForNode) {
                // FIXME
            }

            override fun whileStatement(n: WhileNode) {
                // FIXME
            }

            override fun doStatement(n: DoNode) {
                // FIXME
            }

            override fun breakStatement(n: BreakNode) {
                // FIXME
            }

            override fun continueStatement(n: ContinueNode) {
                // FIXME
            }

            override fun unaryOperator(n: UnaryOperatorNode) {
                // FIXME
            }

            override fun binaryOperator(n: BinaryOperatorNode) {
                // FIXME
            }

            override fun ternaryOperator(n: TernaryOperatorNode) {
                // FIXME
            }

            override fun charLiteral(n: CharacterLiteralNode) {
                // FIXME
            }

            override fun stringLiteral(n: StringLiteralNode) {
                // FIXME
            }

            override fun numberLiteral(n: NumberLiteralNode) {
                // FIXME
            }

            override fun booleanLiteral(n: BooleanLiteralNode) {
                // FIXME
            }

            override fun nullLiteral(n: NullLiteralNode) {
                // FIXME
            }

            override fun variableAccess(n: VariableAccessNode) {
                // FIXME
            }

            override fun arrayAccess(n: ArrayAccessNode) {
                // FIXME
            }

            override fun fieldAccess(n: FieldAccessNode) {
                // FIXME
            }

            override fun functionCall(n: FunctionCallNode) {
                // FIXME
            }

            override fun parenthesis(n: ParenthesisNode) {
                n.setAnnotation(n.expr.getAnnotation(TypeInfo::class.java), TypeInfo::class.java)
            }
        });
    }
}

fun typeToString(t: TypeInfo): String {
    when (t) {
        is NamedType -> return typeToString(t.type)
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
        else -> throw RuntimeException("Unknown TypeInfo subtype ${t}")
    }
}

fun typesEqual(a: TypeInfo, b: TypeInfo): Boolean {
    return true
}
