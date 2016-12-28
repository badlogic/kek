package kek.runtime

data class Source(val location: String, val sourceCode: CharSequence)

data class Location(val source: Source, val line: Int, val column: Int)

data class Structure(val location: Location, val module: String, val name: String, val type: StructureType)

data class Function(val location: Location, val module: String, val name: String, val extern: Boolean, val type: FunctionType)

data class Module(val name: String,
                  val imports: MutableSet<String> = mutableSetOf(),
                  val functions: MutableMap<String, MutableList<Function>> = mutableMapOf(),
                  val structures: MutableMap<String, Structure> = mutableMapOf(),
                  val types: MutableMap<String, TypeInfo> = mutableMapOf()) {
}

fun printModule(module: Module): String {
    val buffer = StringBuffer()

    buffer.appendln("Module ${module.name}")
    buffer.appendln("\tImports")
    for (i in module.imports)
        buffer.appendln("\t\t${i}")
    buffer.appendln("\tStructures")
    for (s in module.structures.values)
        buffer.appendln("\t\t${s.name}")
    buffer.appendln("\tTypes")
    for (t in module.types.values)
        buffer.append("\t\t${t}")
    buffer.appendln("\tFunctions")
    for (fl in module.functions.values)
        for (f in fl)
            buffer.appendln("\t\t${f.name}")
    return buffer.toString()
}