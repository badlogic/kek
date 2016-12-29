package kek.runtime

import kek.compiler.CompilerException
import java.sql.Struct

data class Module(val name: String,
                  private val functions: MutableMap<String, MutableList<FunctionType>> = mutableMapOf(),
                  private val primitiveTypes: MutableMap<String, PrimitiveType> = mutableMapOf(),
                  private val structures: MutableMap<String, StructureType> = mutableMapOf()) {

    fun functions(): Map<String, List<FunctionType>> {
        return functions
    }

    fun addFunction(name: String, func: FunctionType) {
        var funcs = functions[name]
        if (funcs == null) {
            funcs = mutableListOf()
            functions[name] = funcs
        }
        funcs.add(func)
    }

    fun primitiveTypes(): Map<String, PrimitiveType> {
        return primitiveTypes
    }

    fun addPrimitiveType(name: String, prim: PrimitiveType) {
        primitiveTypes.put(name, prim)
    }

    fun structures(): Map<String, StructureType> {
        return structures
    }

    fun addStructure(name: String, struct: StructureType) {
        if (structures.containsKey(name)) {
            val otherStruct = structures[name]!!
            throw RuntimeException("Structure ${this.name}.${name} already defined in ${otherStruct.location.source.location}:(${otherStruct.location.line}, ${otherStruct.location.column})")
        }
        structures[struct.name] = struct
    }

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

    fun lookupStructure(name: String): StructureType? {
        val moduleName = moduleNameFromFQName(name)
        val strippedName = stripModuleName(name)
        if (!moduleName.isEmpty() and !moduleName.equals(this.name)) return null

        return this.structures[strippedName]
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