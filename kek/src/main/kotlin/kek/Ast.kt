package kek

abstract class AstNode (val firstToken: Token, val lastToken: Token){
    val annotations = mutableMapOf<Class<Any>, Any> ()

    fun <T> getAnnotation(cls:Class<T>): T {
        return annotations[cls as Class<Any>] as T
    }

    fun <T> setAnnotation(annotation: T, cls:Class<T>) {
        annotations[cls as Class<Any>] = annotation as Any
    }
}

class CompilationUnit(val nameSpace: String = "", val imports: List<Import>, val functions: List<FunctionDefinition>, val structs: List<StructureDefinition>, firstToken: Token, lastToken: Token) : AstNode(firstToken, lastToken) {
}

class Import(val importName: String, firstToken: Token, lastToken: Token): AstNode(firstToken, lastToken) {
}

class StructureDefinition(val name: Token, val fields: List<VariableDeclaration>, firstToken: Token, lastToken: Token) : AstNode(firstToken, lastToken) {
}

class FunctionDefinition(val name: Token, val parameters: List<Parameter>, val returnType: Type, val body: List<Statement>, firstToken: Token, lastToken: Token) : AstNode(firstToken, lastToken) {
}

class Parameter(val name: Token, val type: Type, firstToken: Token, lastToken: Token) : AstNode(firstToken, lastToken) {
}

open class Type(val name: List<Token>, val isArray:Boolean, val isOptional:Boolean, firstToken: Token, lastToken: Token) : AstNode(firstToken, lastToken) {

    fun fullyQualfiedName():String {
        val buffer = StringBuffer()
        for (i in name.indices) {
            buffer.append(name[i].text)
            if (i < name.size - 1) buffer.append(".")
        }
        return buffer.toString()
    }
}

class NoType (firstToken: Token, lastToken: Token) : Type(emptyList<Token>(), false, false, firstToken, lastToken) {
}

abstract class Statement(firstToken: Token, lastToken: Token) : AstNode(firstToken, lastToken) {
}

class VariableDeclaration(val name: Token, val type: Type, val initializer: Expression, firstToken: Token, lastToken: Token) : Statement(firstToken, lastToken) {
}

class ReturnStatement(val expression: Expression, firstToken: Token, lastToken: Token) : Statement(firstToken, lastToken) {
}

class IfStatement(val condition: Expression, val trueBody: List<Statement>, val elseIfs: List<IfStatement>, val falseBody: List<Statement>, firstToken: Token, lastToken: Token) : Statement(firstToken, lastToken) {
}

class ForStatement(val initializer: List<VariableDeclaration>, val condition: Expression, val increment: List<Expression>, val body: List<Statement>, firstToken: Token, lastToken: Token) : Statement(firstToken, lastToken) {
}

class WhileStatement(val condition: Expression, val body: List<Statement>, firstToken: Token, lastToken: Token) : Statement(firstToken, lastToken) {
}

class DoStatement(val condition: Expression, val body: List<Statement>, firstToken: Token, lastToken: Token) : Statement(firstToken, lastToken) {
}

class BreakStatement(firstToken: Token, lastToken: Token) : Statement(firstToken, lastToken) {
}

class ContinueStatement(firstToken: Token, lastToken: Token) : Statement(firstToken, lastToken) {
}

abstract class Expression(firstToken: Token, lastToken: Token) : Statement(firstToken, lastToken) {
}

class EmptyExpression(firstToken: Token, lastToken: Token) : Expression(firstToken, lastToken) {
}

class UnaryOperator(val opType: Token, val expr: Expression, firstToken: Token, lastToken: Token) : Expression(firstToken, lastToken) {
}

class BinaryOperator(val opType: Token, val left: Expression, val right: Expression, firstToken: Token, lastToken: Token) : Expression(firstToken, lastToken) {
}

class TernaryOperator(val left: Expression, val middle: Expression, val right: Expression, firstToken: Token, lastToken: Token) : Expression(firstToken, lastToken) {
}

class CharacterLiteral(val literal: Token, firstToken: Token, lastToken: Token) : Expression(firstToken, lastToken) {
}

class StringLiteral(val literal: Token, firstToken: Token, lastToken: Token) : Expression(firstToken, lastToken) {
}

class NumberLiteral(val literal: Token, firstToken: Token, lastToken: Token) : Expression(firstToken, lastToken) {
}

class BooleanLiteral(val literal: Token, firstToken: Token, lastToken: Token) : Expression(firstToken, lastToken) {
}

class VariableAccess(val varName: Token, firstToken: Token, lastToken: Token) : Expression(firstToken, lastToken) {
}

class ArrayAccess(val array: Expression, val index: Expression, firstToken: Token, lastToken: Token) : Expression(firstToken, lastToken) {
}

class FieldAccess(val base: Expression, val varName: Token, firstToken: Token, lastToken: Token) : Expression(firstToken, lastToken) {
}

class FunctionCall(val function: Expression, val arguments: List<Expression>, firstToken: Token, lastToken: Token) : Expression(firstToken, lastToken) {
}

// needed for formatting
class Parenthesis(val expr: Expression, firstToken: Token, lastToken: Token) : Expression(firstToken, lastToken) {
}

interface AstVisitor {
    fun namespace(namespace: String)
    fun compilationUnit(n: CompilationUnit)
    fun structure(n: StructureDefinition)
    fun function(n: FunctionDefinition)
    fun variableDeclaration(n: VariableDeclaration)
    fun returnStatement(n: ReturnStatement)
    fun ifStatement(n: IfStatement)
    fun forStatement(n: ForStatement)
    fun whileStatement(n: WhileStatement)
    fun doStatement(n: DoStatement)
    fun breakStatement(n: BreakStatement)
    fun continueStatement(n: ContinueStatement)
    fun unaryOperator(n: UnaryOperator)
    fun binaryOperator(n: BinaryOperator)
    fun ternaryOperator(n: TernaryOperator)
    fun charLiteral(n: CharacterLiteral)
    fun stringLiteral(n: StringLiteral)
    fun numberLiteral(n: NumberLiteral)
    fun booleanLiteral(n: BooleanLiteral)
    fun variableAccess(n: VariableAccess)
    fun arrayAccess(n: ArrayAccess)
    fun fieldAccess(n: FieldAccess)
    fun functionCall(n: FunctionCall)
    fun parenthesis(n: Parenthesis)
}

fun traverseAstDepthFirst(ast: AstNode, visitor: AstVisitor) {
    when(ast) {
        is CompilationUnit -> {
            visitor.namespace(ast.nameSpace)

            for (s in ast.structs) {
                traverseAstDepthFirst(s, visitor)
                visitor.structure(s)
            }

            for (f in ast.functions) {
                traverseAstDepthFirst(f, visitor)
            }

            visitor.compilationUnit(ast)
        }
        is StructureDefinition -> {
            visitor.structure(ast)
        }
        is FunctionDefinition -> {
            for (s in ast.body) traverseAstDepthFirst(s, visitor)
            visitor.function(ast)
        }
        is VariableDeclaration -> {
            visitor.variableDeclaration(ast)
        }
        is ReturnStatement -> {
            visitor.returnStatement(ast)
        }
        is IfStatement -> {
            traverseAstDepthFirst(ast.condition, visitor)
            for (s in ast.trueBody) traverseAstDepthFirst(s, visitor)
            for (e in ast.elseIfs) traverseAstDepthFirst(e, visitor)
            for (s in ast.falseBody) traverseAstDepthFirst(s, visitor)
            visitor.ifStatement(ast)
        }
        is ForStatement -> {
            for (s in ast.initializer) traverseAstDepthFirst(s, visitor)
            traverseAstDepthFirst(ast.condition, visitor)
            for (s in ast.increment) traverseAstDepthFirst(s, visitor)
            for (s in ast.body) traverseAstDepthFirst(s, visitor)
            visitor.forStatement(ast)
        }
        is WhileStatement -> {
            traverseAstDepthFirst(ast.condition, visitor)
            for (s in ast.body) traverseAstDepthFirst(s, visitor)
            visitor.whileStatement(ast)
        }
        is DoStatement -> {
            for (s in ast.body) traverseAstDepthFirst(s, visitor)
            traverseAstDepthFirst(ast.condition, visitor)
            visitor.doStatement(ast)
        }
        is BreakStatement -> {
            visitor.breakStatement(ast)
        }
        is ContinueStatement -> {
            visitor.continueStatement(ast)
        }
        is UnaryOperator -> {
            traverseAstDepthFirst(ast.expr, visitor)
            visitor.unaryOperator(ast)
        }
        is BinaryOperator -> {
            traverseAstDepthFirst(ast.left, visitor)
            traverseAstDepthFirst(ast.right, visitor)
            visitor.binaryOperator(ast)
        }
        is TernaryOperator -> {
            traverseAstDepthFirst(ast.left, visitor)
            traverseAstDepthFirst(ast.middle, visitor)
            traverseAstDepthFirst(ast.right, visitor)
            visitor.ternaryOperator(ast)
        }
        is CharacterLiteral -> {
            visitor.charLiteral(ast)
        }
        is StringLiteral -> {
            visitor.stringLiteral(ast)
        }
        is NumberLiteral -> {
            visitor.numberLiteral(ast)
        }
        is BooleanLiteral -> {
            visitor.booleanLiteral(ast)
        }
        is VariableAccess -> {
            visitor.variableAccess(ast)
        }
        is ArrayAccess -> {
            traverseAstDepthFirst(ast.index, visitor)
            traverseAstDepthFirst(ast.array, visitor)
            visitor.arrayAccess(ast)
        }
        is FieldAccess -> {
            traverseAstDepthFirst(ast.base, visitor)
            visitor.fieldAccess(ast)
        }
        is FunctionCall -> {
            traverseAstDepthFirst(ast.function, visitor)
            visitor.functionCall(ast)
        }
        is Parenthesis -> {
            traverseAstDepthFirst(ast.expr, visitor)
            visitor.parenthesis(ast)
        }
        else -> throw Exception("Unhandled AST node")
    }
}


var i = 0

fun printAst(cu: CompilationUnit): String {
    val buffer = StringBuffer()
    val nodes = StringBuffer()
    val edges = StringBuffer()

    buffer.append("digraph cu {\n")
    nodes.append("node[shape=box]\n")
    nodes.append("cu [label=\"Compilation Unit\", shape=box]\n")
    nodes.append("ns [label=\"Namespace: ${cu.nameSpace}\"]\n")
    edges.append("cu -> ns\n")

    for (i in cu.imports) printAstNode("cu", i, nodes, edges)
    for (s in cu.structs) printAstNode("cu", s, nodes, edges)
    for (f in cu.functions) printAstNode("cu", f, nodes, edges)

    buffer.append(nodes)
    buffer.append(edges)
    buffer.append("}\n")
    return buffer.toString()
}

fun printAstNode(p: String, n: AstNode, nodes: StringBuffer, edges: StringBuffer): String {
    if (n is FunctionDefinition) return printFunctionDefinition(n, nodes, edges)
    else if (n is StructureDefinition) return printStructureDefinition(n, nodes, edges)
    else if (n is Parameter) return printParameter(p, n, nodes, edges)
    else if (n is UnaryOperator) return printUnaryOperator(p, n, nodes, edges)
    else if (n is BinaryOperator) return printBinaryOperator(p, n, nodes, edges)
    else if (n is CharacterLiteral) return printCharacterLiteral(p, n, nodes, edges)
    else if (n is StringLiteral) return printStringLiteral(p, n, nodes, edges)
    else if (n is NumberLiteral) return printNumberLiteral(p, n, nodes, edges)
    else if (n is BooleanLiteral) return printBooleanLiteral(p, n, nodes, edges)
    else if (n is VariableAccess) return printVariableAccess(p, n, nodes, edges)
    else if (n is ArrayAccess) return printArrayAccess(p, n, nodes, edges)
    else if (n is FieldAccess) return printFieldAccess(p, n, nodes, edges)
    else if (n is FunctionCall) return printFunctionCall(p, n, nodes, edges)
    else if (n is VariableDeclaration) return printVariableDeclaration(p, n, nodes, edges)
    else if (n is IfStatement) return printIfStatement(p, n, nodes, edges)
    else if (n is ForStatement) return printForStatement(p, n, nodes, edges)
    else if (n is WhileStatement) return printWhileStatement(p, n, nodes, edges)
    else if (n is DoStatement) return printDoStatement(p, n, nodes, edges)
    else if (n is ReturnStatement) return printReturnStatement(p, n, nodes, edges)
    else if (n is BreakStatement) return printBreakStatement(p, n, nodes, edges)
    else if (n is ContinueStatement) return printContinueStatement(p, n, nodes, edges)
    else if (n is TernaryOperator) return printTernaryOperator(p, n, nodes, edges)
    else if (n is EmptyExpression) return "empty"
    else if (n is Parenthesis) return printParanthesis(p, n, nodes, edges)
    else if (n is Import) return printImport(p, n, nodes, edges)
    else throw RuntimeException("Unknown AST node $n")
}

fun  printImport(p: String, n: Import, nodes: StringBuffer, edges: StringBuffer): String {
    val name = "b${i++}"
    nodes.append("$name [label=\"Import ${n.importName}\"]\n")
    edges.append("$p->$name\n")
    return name
}

fun  printParanthesis(p: String, n: Parenthesis, nodes: StringBuffer, edges: StringBuffer): String {
    val name = "b${i++}"
    nodes.append("$name [label=\"()\"]\n")
    edges.append("$p->$name\n")
    printAstNode(name, n.expr, nodes, edges)
    return name
}

fun printContinueStatement(p: String, n: ContinueStatement, nodes: StringBuffer, edges: StringBuffer): String {
    val name = "b${i++}"
    nodes.append("$name [label=\"Continue\"]\n")
    edges.append("$p->$name\n")
    return name
}

fun printBreakStatement(p: String, n: BreakStatement, nodes: StringBuffer, edges: StringBuffer): String {
    val name = "b${i++}"
    nodes.append("$name [label=\"Break\"]\n")
    edges.append("$p->$name\n")
    return name
}

fun printWhileStatement(p: String, n: WhileStatement, nodes: StringBuffer, edges: StringBuffer): String {
    val name = "b${i++}"
    nodes.append("$name [label=\"While\"]\n")
    edges.append("$p->$name\n")
    printAstNode(name, n.condition, nodes, edges)
    printAstNodeList(name, n.body, nodes, edges)
    return name
}

fun printDoStatement(p: String, n: DoStatement, nodes: StringBuffer, edges: StringBuffer): String {
    val name = "b${i++}"
    nodes.append("$name [label=\"Do\"]\n")
    edges.append("$p->$name\n")
    printAstNode(name, n.condition, nodes, edges)
    printAstNodeList(name, n.body, nodes, edges)
    return name
}

fun printForStatement(p: String, n: ForStatement, nodes: StringBuffer, edges: StringBuffer): String {
    val name = "b${i++}"
    nodes.append("$name [label=\"For\"]\n")
    edges.append("$p->$name\n")
    printAstNodeList(name, n.initializer, nodes, edges)
    printAstNode(name, n.condition, nodes, edges)
    printAstNodeList(name, n.increment, nodes, edges)
    printAstNodeList(name, n.body, nodes, edges)
    return name
}

fun printReturnStatement(p: String, n: ReturnStatement, nodes: StringBuffer, edges: StringBuffer): String {
    val name = "b${i++}"
    nodes.append("$name [label=\"Return\"]\n")
    edges.append("$p->$name\n")
    printAstNode(name, n.expression, nodes, edges)
    return name
}

fun printAstNodeList(p: String, l: List<AstNode>, nodes: StringBuffer, edges: StringBuffer): String {
    var last = p
    for (s in l) {
        last = printAstNode(last, s, nodes, edges)
    }
    return last
}

fun printIfStatement(p: String, n: IfStatement, nodes: StringBuffer, edges: StringBuffer): String {
    val name = "b${i++}"
    nodes.append("$name [label=\"If\"]\n")
    edges.append("$p->$name\n")
    printAstNode(name, n.condition, nodes, edges)
    printAstNodeList(name, n.trueBody, nodes, edges)
    printAstNodeList(name, n.elseIfs, nodes, edges)
    printAstNodeList(name, n.falseBody, nodes, edges)
    return name
}

fun printTernaryOperator(p: String, n: TernaryOperator, nodes: StringBuffer, edges: StringBuffer): String {
    val name = "b${i++}"
    nodes.append("$name [label=\"Ternary Operator\"]\n")
    edges.append("$p->$name\n")
    printAstNode(name, n.left, nodes, edges)
    printAstNode(name, n.middle, nodes, edges)
    printAstNode(name, n.right, nodes, edges)
    return name
}

fun printVariableDeclaration(p: String, n: VariableDeclaration, nodes: StringBuffer, edges: StringBuffer): String {
    val name = "b${i++}"
    nodes.append("$name [label=\"Variable ${n.name.text}, ${n.type.fullyQualfiedName()}, optional: ${n.type.isOptional}, array: ${n.type.isArray}\"]\n")
    edges.append("$p->$name\n")
    printAstNode(name, n.initializer, nodes, edges)
    return name
}

fun printFunctionCall(p: String, n: FunctionCall, nodes: StringBuffer, edges: StringBuffer): String {
    val name = "b${i++}"
    nodes.append("$name [label=\"Call ()\"]\n")
    edges.append("$p->$name\n")
    printAstNode(name, n.function, nodes, edges)
    for (a in n.arguments) printAstNode(name, a, nodes, edges)
    return name
}

fun printFieldAccess(p: String, n: FieldAccess, nodes: StringBuffer, edges: StringBuffer): String {
    val name = "b${i++}"
    nodes.append("$name [label=\"Field Access .${n.varName.text}\"]\n")
    edges.append("$p->$name\n")
    printAstNode(name, n.base, nodes, edges)
    return name
}

fun printArrayAccess(p: String, n: ArrayAccess, nodes: StringBuffer, edges: StringBuffer): String {
    val name = "b${i++}"
    nodes.append("$name [label=\"Array Access []\"]\n")
    edges.append("$p->$name\n")
    printAstNode(name, n.array, nodes, edges)
    printAstNode(name, n.index, nodes, edges)
    return name
}

fun printVariableAccess(p: String, n: VariableAccess, nodes: StringBuffer, edges: StringBuffer): String {
    val name = "s${i++}"
    nodes.append("$name [label=\"Variable access '${n.varName.text}'\"]\n")
    edges.append("$p->$name\n")
    return name
}

fun printBooleanLiteral(p: String, n: BooleanLiteral, nodes: StringBuffer, edges: StringBuffer): String {
    val name = "s${i++}"
    nodes.append("$name [label=\"Boolean '${n.literal.text}'\"]\n")
    edges.append("$p->$name\n")
    return name
}

fun printNumberLiteral(p: String, n: NumberLiteral, nodes: StringBuffer, edges: StringBuffer): String {
    val name = "s${i++}"
    nodes.append("$name [label=\"Number '${n.literal.text}'\"]\n")
    edges.append("$p->$name\n")
    return name
}

fun printStringLiteral(p: String, n: StringLiteral, nodes: StringBuffer, edges: StringBuffer): String {
    val name = "s${i++}"
    nodes.append("$name [label=\"String '${n.literal.text}'\"]\n")
    edges.append("$p->$name\n")
    return name
}

fun printCharacterLiteral(p: String, n: CharacterLiteral, nodes: StringBuffer, edges: StringBuffer): String {
    val name = "s${i++}"
    nodes.append("$name [label=\"Character '${n.literal.text}'\"]\n")
    edges.append("$p->$name\n")
    return name
}

fun printBinaryOperator(p: String, n: BinaryOperator, nodes: StringBuffer, edges: StringBuffer): String {
    val name = "b${i++}"
    nodes.append("$name [label=\"Binary ${n.opType.text}\"]\n")
    edges.append("$p->$name\n")
    printAstNode(name, n.left, nodes, edges)
    printAstNode(name, n.right, nodes, edges)
    return name
}

fun printUnaryOperator(p: String, n: UnaryOperator, nodes: StringBuffer, edges: StringBuffer): String {
    val name = "n${i++}"
    nodes.append("$name [label=\"Unary ${n.opType.text}\"]\n")
    edges.append("$p->$name\n")
    printAstNode(name, n.expr, nodes, edges)
    return name
}

fun printParameter(p: String, n: Parameter, nodes: StringBuffer, edges: StringBuffer): String {
    val name = "p${i++}"
    nodes.append("$name [label=\"Param: ${n.name.text}, ${n.type.fullyQualfiedName()}\"]\n")
    edges.append("$p->$name\n")
    return name
}

fun printStructureDefinition(n: StructureDefinition, nodes: StringBuffer, edges: StringBuffer): String {
    val name = "s${i++}"
    nodes.append("$name [label=\"Structure: ${n.name.text}\"]\n")
    edges.append("cu->$name\n")
    for (f in n.fields) printAstNode(name, f, nodes, edges)
    return name
}

fun printFunctionDefinition(n: FunctionDefinition, nodes: StringBuffer, edges: StringBuffer): String {
    val name = "f${i++}"
    nodes.append("$name [label=\"Function: ${n.name.text}, return: ${n.returnType.fullyQualfiedName()}\"]\n")
    edges.append("cu->$name\n")
    for (p in n.parameters) printAstNode(name, p, nodes, edges)

    var last = name
    for (s in n.body) {
        last = printAstNode(last, s, nodes, edges)
    }

    return name
}
