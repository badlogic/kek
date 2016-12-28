package kek.runtime

import java.sql.Struct

data class Module(val name: String,
                  val imports: MutableSet<String> = mutableSetOf(),
                  val functions: MutableMap<String, MutableList<FunctionType>> = mutableMapOf(),
                  val primitiveTypes: MutableMap<String, PrimitiveType> = mutableMapOf(),
                  val structures: MutableMap<String, StructureType> = mutableMapOf()) {

    fun lookup(name: String): List<TypeInfo> {
        val moduleName = moduleNameFromFQName(name)
        if (!moduleName.isEmpty() and !moduleName.equals(this.name)) return emptyList()

        val result = mutableListOf<TypeInfo>()
        if (primitiveTypes.containsKey(name)) result.add(primitiveTypes[name]!!)
        if (functions.containsKey(name)) result.addAll(functions[name]!!)
        if (structures.containsKey(name)) result.add(structures[name]!!)
        return result
    }

    fun lookupFunction(name: String): List<FunctionType> {
        val moduleName = moduleNameFromFQName(name)
        val strippedName = stripModuleName(name)
        if (!moduleName.isEmpty() and !moduleName.equals(this.name)) return emptyList()

        val result = this.functions[strippedName]
        if (result == null) return emptyList()
        else return result
    }

    fun lookupPrimitiveOrStructure(name: String): List<TypeInfo> {
        val moduleName = moduleNameFromFQName(name)
        val strippedName = stripModuleName(name)
        if (!moduleName.isEmpty() and !moduleName.equals(this.name)) return emptyList()

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