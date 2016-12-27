package kek.runtime

data class Source(val location: String, val sourceCode: CharSequence)

data class Location(val source: Source, val line: Int, val column: Int)

data class Structure(val location: Location, val module: String, val name: String)

data class Function(val location: Location, val module: String, val name: String)

data class Module(val name: String, val imports: MutableSet<String> = mutableSetOf(), val functions: MutableMap<String, Function> = mutableMapOf(), val structures: MutableMap<String, Structure> = mutableMapOf())

fun printModule(module: Module): String {
    val buffer = StringBuffer()

    buffer.appendln("Module ${module.name}")
    buffer.appendln("\tImports")
    for (i in module.imports)
        buffer.appendln("\t\t${i}")
    buffer.appendln("\tStructures")
    for (s in module.structures.values)
        buffer.appendln("\t\t${s.name}")
    buffer.appendln("\tFunctions")
    for (f in module.functions.values)
        buffer.appendln("\t\t${f.name}")
    return buffer.toString()
}