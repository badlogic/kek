package kek.compiler

import kek.runtime.*

class SymbolTable {
    private val tables = mutableListOf<MutableMap<String, TypeInfo>>()

    constructor() {
        push()
    }

    fun push() {
        tables.add(mutableMapOf<String, TypeInfo>())
    }

    fun pop() {
        tables.removeAt(tables.lastIndex)
    }

    fun add(name: String, type: TypeInfo) {
        tables.last()[name] = type
    }

    fun contains(name: String): Boolean {
        for (i in (tables.size - 1) downTo 0) {
            return tables[i].containsKey(name)
        }
        return false
    }

    fun get(name: String): TypeInfo? {
        for (i in (tables.size - 1) downTo 0) {
            if (tables[i].containsKey(name)) return tables[i].get(name)
        }
        return null
    }
}

class CompilerException(val source: Source,
                        val msg: String,
                        val line: Int,
                        val columnStart: Int,
                        val columnEnd: Int) : RuntimeException(msg) {

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
                         val modules: MutableMap<String, Module> = mutableMapOf<String, Module>(),
                         val globalTypes: GlobalTypes = GlobalTypes())

fun compile(sources: List<Source>): CompilerState {
    val state = CompilerState()

    for (source in sources) {
        state.compilationUnits.add(parse(source))
    }

    gatherModules(state)
    resolveTopLevelTypes(state)

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
                throw CompilerException(cu.source, "Structure ${module.name}.${s.name.text} already defined in ${otherStruct.location.source.location}:(${otherStruct.location.line}, ${otherStruct.location.column})", s.name.line, s.name.column, s.name.column + s.name.text.length)
            }

            val struct = Structure(Location(cu.source, s.name.line, s.name.column), module.name, s.name.text, UnknownStructureType)
            s.setAnnotation(struct, Structure::class.java)
            module.structures[struct.name] = struct
        }

        for (f in cu.functions) {
            val func = Function(Location(cu.source, f.name.line, f.name.column), module.name, f.name.text, f.extern, UnknownFunctionType)
            var funcs = module.functions[func.name]
            if (funcs == null) {
                funcs = mutableListOf()
                module.functions[func.name] = funcs
            }
            f.setAnnotation(func, kek.runtime.Function::class.java)
            funcs!!.add(func)
        }
    }
}

/**
 * Resolves the top level structure and function types and their
 * fields and parameters. Does not resolve types referenced in
 * function bodies.
 */
private fun resolveTopLevelTypes(state: CompilerState) {
    for (cu in state.compilationUnits) {
        traverseAstDepthFirst(cu, object: AstVisitorAdapter() {
            override fun structure(n: StructureNode) {
                print(n.name)
            }

            override fun function(n: FunctionNode) {
                print(n.name)
            }
        }, setOf<Class<out AstNode>>(CompilationUnitNode::class.java, StructureNode::class.java, FunctionNode::class.java));
    }
}
