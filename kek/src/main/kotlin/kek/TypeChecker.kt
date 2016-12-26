package kek

interface TypeInfo

class BasicType(val namespace: String, val name: String) : TypeInfo

class ArrayType(val elementType: TypeInfo) : TypeInfo

class OptionalType(val elementType: TypeInfo) : TypeInfo

class FunctionType(val namespace: String, val name: String, val parameters: List<TypeInfo>, val returnType: TypeInfo) : TypeInfo

class FieldType(val name: String, type: TypeInfo) : TypeInfo

class StructType(val namespace: String, val name: String, val fields: List<FieldType> = mutableListOf<FieldType>()) : TypeInfo

val VoidType = BasicType("kek", "void")
val IntType = BasicType("kek", "int")
val FloatType = BasicType("kek", "float")

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

data class TypeCheckerState(val symbolTable: SymbolTable = SymbolTable())

fun typeCheck(compilationUnits: List<CompilationUnit>): TypeCheckerState {
    val state = TypeCheckerState()

    return state
}

private fun gatherGlobals(compilationUnits: List<CompilationUnit>) {
    
}