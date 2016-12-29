package kek.compiler

import kek.runtime.*

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
    defaultModule.addPrimitiveType(Int8Type.name, Int8Type)
    defaultModule.addPrimitiveType(Int16Type.name, Int16Type)
    defaultModule.addPrimitiveType("int", Int32Type)
    defaultModule.addPrimitiveType(Int32Type.name, Int32Type)
    defaultModule.addPrimitiveType(Int64Type.name, Int64Type)
    defaultModule.addPrimitiveType(FloatType.name, FloatType)
    defaultModule.addPrimitiveType(DoubleType.name, DoubleType)
    defaultModule.addPrimitiveType(BooleanType.name, BooleanType)
    defaultModule.addPrimitiveType(VoidType.name, VoidType)
    state.modules.put("", defaultModule)

    for (source in sources) {
        state.compilationUnits.add(parse(source))
    }

    gatherModules(state)
    checkTypes(state)

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

        for (s in cu.structs) {
            val otherStruct = module.lookupStructure(s.name.text)
            if (otherStruct != null) {
                throw CompilerException(cu.source, "Structure ${module.name}.${s.name.text} already defined in ${otherStruct.location.source.location}:(${otherStruct.location.line}, ${otherStruct.location.column})", s.name)
            }

            val struct = StructureType(Location(cu.source, s.name.line, s.name.column), module.name, s.name.text)
            s.setAnnotation(struct, StructureType::class.java)
            module.addStructure(struct.name, struct)
        }

        for (f in cu.functions) {
            val func = FunctionType(Location(cu.source, f.name.line, f.name.column), module.name, f.name.text, f.extern)
            f.setAnnotation(func, kek.runtime.FunctionType::class.java)
            module.addFunction(func.name, func)
        }
    }
}
