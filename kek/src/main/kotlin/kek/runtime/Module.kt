package kek.runtime

import java.sql.Struct

data class Module(val name: String,
                  val imports: MutableSet<String> = mutableSetOf(),
                  val functions: MutableMap<String, MutableList<FunctionType>> = mutableMapOf(),
                  val primitiveTypes: MutableMap<String, PrimitiveType> = mutableMapOf(),
                  val structures: MutableMap<String, StructureType> = mutableMapOf()) {

    fun lookup(name: String): List<TypeInfo> {
        val moduleName = moduleNameFromFQName(name)
        if (!moduleName.isEmpty() and !moduleName.equals(name)) return emptyList()

        val result = mutableListOf<TypeInfo>()
        if (primitiveTypes.containsKey(name)) result.add(primitiveTypes[name]!!)
        if (functions.containsKey(name)) result.addAll(functions[name]!!)
        if (structures.containsKey(name)) result.add(structures[name]!!)
        return result
    }

    fun lookupFunction(name: String): List<FunctionType> {
        val moduleName = moduleNameFromFQName(name)
        val strippedName = stripModuleName(name)
        if (!moduleName.isEmpty() and !moduleName.equals(name)) return emptyList()

        val result = this.functions[strippedName]
        if (result == null) return emptyList()
        else return result
    }

    fun lookupPrimitiveOrStructure(name: String): List<TypeInfo> {
        val moduleName = moduleNameFromFQName(name)
        val strippedName = stripModuleName(name)
        if (!moduleName.isEmpty() and !moduleName.equals(name)) return emptyList()

        val result = mutableListOf<TypeInfo>()
        val primitive = this.primitiveTypes[strippedName]
        if (primitive != null) result.add(primitive)
        val struct = this.structures[strippedName]
        if (struct != null) result.add(struct)
        return result
    }
}

fun moduleNameFromFQName(nameFQ: String): String {
    val moduleName: String
    val idx = nameFQ.lastIndexOf(".")
    if (idx < 0) return ""
    else return nameFQ.substring(0, idx)
}

fun stripModuleName(nameFQ: String): String {
    val idx = nameFQ.lastIndexOf(".")
    return if (idx < 0) nameFQ else nameFQ.substring(idx + 1)
}

fun printModule(module: Module): String {
    val buffer = StringBuffer()

    buffer.appendln("Module ${module.name}")
    buffer.appendln("\tImports")
    for (i in module.imports)
        buffer.appendln("\t\t${i}")
    buffer.appendln("\tPrimitive Types")
    for (t in module.primitiveTypes.values)
        buffer.appendln("\t\t${t}")
    buffer.appendln("\tStructures")
    for (s in module.structures.values) {
        buffer.appendln("\t\t${s.name}")
        for (f in s.fields) {
            buffer.appendln("\t\t\t${f.name}: ${f.type}")
        }
        buffer.appendln()
    }
    buffer.appendln("\tFunctions")
    for (fl in module.functions.values)
        for (f in fl) {
            buffer.appendln("\t\t${f.name}")
            for (p in f.parameters) {
                buffer.appendln("\t\t\t${p.name}: ${p.type}")
            }
            buffer.appendln("\t\t\treturn: ${f.returnType}")
        }
    return buffer.toString()
}