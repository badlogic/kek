package kek.runtime

data class Source(val location: String, val sourceCode: CharSequence)

data class Location(val source: Source, val line: Int, val column: Int)

interface TypeInfo {
}

data class PrimitiveType(val name: String, val isNumeric: Boolean) : TypeInfo

data class ArrayType(val elementType: TypeInfo) : TypeInfo

data class OptionalType(val elementType: TypeInfo) : TypeInfo

data class NamedType(val name: String, val type: TypeInfo)

data class StructureType(val location: Location, val module: String, val name: String,
                         val fields: MutableList<NamedType> = mutableListOf<NamedType>(), val functions: MutableList<FunctionType> = mutableListOf<FunctionType>()) : TypeInfo {
    override fun toString(): String {
        return "StructureType(module=${module}, name=${name})"
    }

    fun fullyQualifiedName(): String {
        return if (module.isEmpty()) name else "${module}.${name}"
    }
}

enum class FunctionUsage {
    Function,
    Initializer,
    Constructor,
    Method,
}

data class FunctionType(val location: Location, val module: String, val name: String, val extern: Boolean,
                        val parameters: MutableList<NamedType> = mutableListOf<NamedType>(), var returnType: TypeInfo = UnknownType, val usage: FunctionUsage = FunctionUsage.Function) : TypeInfo

val Int8Type = PrimitiveType("int8", true)
val Int16Type = PrimitiveType("int16", true)
val Int32Type = PrimitiveType("int32", true)
val Int64Type = PrimitiveType("int64", true)
val FloatType = PrimitiveType("float", true)
val DoubleType = PrimitiveType("double", true)
val BooleanType = PrimitiveType("boolean", false)
val VoidType = PrimitiveType("void", false)
val NullType = PrimitiveType("null", false)

val UnknownType = PrimitiveType("Unknown", false)