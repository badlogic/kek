package kek.runtime

data class Source(val location: String, val sourceCode: CharSequence)

data class Location(val source: Source, val line: Int, val column: Int)

interface TypeInfo {
}

data class PrimitiveType(val name: String) : TypeInfo

data class ArrayType(val elementType: TypeInfo) : TypeInfo

data class OptionalType(val elementType: TypeInfo) : TypeInfo

data class NamedType(val name: String, val type: TypeInfo) : TypeInfo

data class StructureType(val location: Location, val module: String, val name: String,
                         val fields: MutableList<NamedType> = mutableListOf<NamedType>()) : TypeInfo

data class FunctionType(val location: Location, val module: String, val name: String, val extern: Boolean,
                        val parameters: MutableList<NamedType> = mutableListOf<NamedType>(), var returnType: TypeInfo = UnknownType) : TypeInfo

val IntType = PrimitiveType("int")
val Int8Type = PrimitiveType("int8")
val Int16Type = PrimitiveType("int16")
val Int32Type = PrimitiveType("int32")
val Int64Type = PrimitiveType("int64")
val FloatType = PrimitiveType("float")
val DoubleType = PrimitiveType("double")
val BooleanType = PrimitiveType("boolean")
val VoidType = PrimitiveType("void")

val UnknownType = PrimitiveType("Unknown")