package kek.runtime

interface TypeInfo

interface ReferenceType

class PrimitiveType(val name: String) : TypeInfo

class ArrayType(val elementType: TypeInfo = UnknownPrimitiveType) : TypeInfo, ReferenceType

class OptionalType(val elementType: TypeInfo = UnknownPrimitiveType) : TypeInfo

class NamedType(val name: String, type: TypeInfo = UnknownPrimitiveType) : TypeInfo

class FunctionType(val parameters: List<TypeInfo> = mutableListOf<NamedType>(), var returnType: TypeInfo = UnknownPrimitiveType) : TypeInfo, ReferenceType

class StructureType(val fields: List<NamedType> = mutableListOf<NamedType>()) : TypeInfo, ReferenceType

val IntType = PrimitiveType("int")
val Int8Type = PrimitiveType("int8")
val Int16Type = PrimitiveType("int16")
val Int32Type = PrimitiveType("int32")
val Int64Type = PrimitiveType("int64")

val FloatType = PrimitiveType("float")
val DoubleType = PrimitiveType("double")

val BooleanType = PrimitiveType("boolean")

val VoidType = PrimitiveType("void")

val UnknownPrimitiveType = PrimitiveType("Unknown")
val UnknownStructureType = StructureType()
val UnknownFunctionType = FunctionType()

class GlobalTypes(val primitiveTypes: MutableMap<String, PrimitiveType> = mutableMapOf(
                        Pair(IntType.name, IntType),
                        Pair(Int8Type.name, Int8Type),
                        Pair(Int16Type.name, Int16Type),
                        Pair(Int32Type.name, Int32Type),
                        Pair(Int64Type.name, Int64Type),
                        Pair(FloatType.name, FloatType),
                        Pair(DoubleType.name, DoubleType),
                        Pair(BooleanType.name, BooleanType),
                        Pair(VoidType.name, VoidType)),
                  val arrayTypes: MutableMap<String, ArrayType> = mutableMapOf(),
                  val optionalTypes: MutableMap<String, OptionalType> = mutableMapOf()) {
}