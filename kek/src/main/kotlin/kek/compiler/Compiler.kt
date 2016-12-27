package kek.compiler

import kek.runtime.*

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

data class CompilerState(val compilationUnits: MutableList<CompilationUnitNode> = mutableListOf<CompilationUnitNode>(), val modules: MutableMap<String, Module> = mutableMapOf<String, Module>())

fun compile(sources: List<Source>): CompilerState {
    val state = CompilerState()

    for (source in sources) {
        state.compilationUnits.add(parse(source))
    }

    gatherModules(state)

    return state
}

private fun gatherModules(state: CompilerState) {
    for (cu in state.compilationUnits) {
        val module: Module
        if (state.modules.containsKey(cu.module)) {
            module = state.modules[cu.module]!!
        } else {
            module = Module(cu.module)
            state.modules[cu.module] = module
        }

        for (i in cu.imports) {
            module.imports.add(i.importName)
        }

        for (s in cu.structs) {
            if (module.structures.containsKey(s.name.text)) {
                val otherStruct = module.structures[s.name.text]!!
                throw CompilerException(cu.source, "Structure ${module.name}.${s.name.text} already defined in ${otherStruct.location.source.location}:(${otherStruct.location.line}, ${otherStruct.location.column})", s.name.line, s.name.column, s.name.column + s.name.text.length)
            }

            val struct = Structure(Location(cu.source, s.name.line, s.name.column), module.name, s.name.text)
            module.structures[struct.name] = struct
        }

        for (f in cu.functions) {
            val func = Function(Location(cu.source, f.name.line, f.name.column), module.name, f.name.text)
            var funcs = module.functions[func.name]
            if (funcs == null) {
                funcs = mutableListOf()
                module.functions[func.name] = funcs
            }
            funcs!!.add(func)
        }
    }
}
