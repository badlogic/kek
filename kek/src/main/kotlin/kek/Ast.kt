package kek

interface AstNode {
}

class CompilationUnit(val nameSpace: String = "", val functions: List<FunctionDefinition>, val structs: List<StructureDefinition>) : AstNode {
}

class StructureDefinition : AstNode {
}

class FunctionDefinition(val identifier: Token, val parameters: List<Parameter>, val returnType: Type, val body: List<Statement>) : AstNode {
}

class Parameter(val identifier: Token, val type: Type) : AstNode {
}

open class Type(val identifier: Token?) : AstNode {
}

class NoType : Type(null) {
}

abstract class Statement : AstNode {
}

class ReturnStatement(val expression: Expression) : Statement() {
}

class IfStatement(val condition: Expression, val trueBody: List<Statement>, val elseIfs: List<IfStatement>, val falseBody: List<Statement>) : Statement() {
}

class ForStatement(val initializer: List<VariableDeclaration>, val condition: Expression, val increment: List<Expression>, val body: List<Statement>) : Statement() {
}

class WhileStatement(val condition: Expression, val body: List<Statement>) : Statement() {
}

class DoStatement(val condition: Expression, val body: List<Statement>) : Statement() {
}

abstract class Expression : Statement() {
}

class EmptyExpression : Expression() {
}

class UnaryOperator(val opType: Token, val expr: Expression) : Expression() {
}

class BinaryOperator(val opType: Token, val left: Expression, val right: Expression) : Expression() {
}

class TernaryOperator(val left: Expression, val middle: Expression, val right: Expression) : Expression() {
}

class CharacterLiteral(val literal: Token) : Expression() {
}

class StringLiteral(val literal: Token) : Expression() {
}

class NumberLiteral(val literal: Token) : Expression() {
}

class BooleanLiteral(val literal: Token) : Expression() {
}

class VariableAccess(val varName: Token) : Expression() {
}

class ArrayAccess(val array: Expression, val index: Expression) : Expression() {
}

class FieldAccess(val base: Expression, val varName: Token) : Expression() {
}

class FunctionCall(val function: Expression, val arguments: List<Expression>) : Expression() {
}

class VariableDeclaration(val name: Token, val type: Type, val initializer: Expression) : Statement() {
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

    for (f in cu.functions) {
        printAstNode("cu", f, nodes, edges)
    }

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
    else if (n is TernaryOperator) return printTernaryOperator(p, n, nodes, edges)
    else if (n is EmptyExpression) return "empty"
    else throw RuntimeException("Unknown AST node $n")
}

fun  printWhileStatement(p: String, n: WhileStatement, nodes: StringBuffer, edges: StringBuffer): String {
    val name = "b${i++}"
    nodes.append("$name [label=\"While\"]\n")
    edges.append("$p->$name\n")
    printAstNode(name, n.condition, nodes, edges)
    printAstNodeList(name, n.body, nodes, edges)
    return name
}

fun  printDoStatement(p: String, n: DoStatement, nodes: StringBuffer, edges: StringBuffer): String {
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
    nodes.append("$name [label=\"Variable ${n.name.text}, ${n.type.identifier?.text}\"]\n")
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
    nodes.append("$name [label=\"Param: ${n.identifier.text}, ${n.type.identifier?.text}\"]\n")
    edges.append("$p->$name\n")
    return name
}

fun printStructureDefinition(n: StructureDefinition, nodes: StringBuffer, edges: StringBuffer): String {
    val name = "s${i++}"
    return name
}

fun printFunctionDefinition(n: FunctionDefinition, nodes: StringBuffer, edges: StringBuffer): String {
    val name = "f${i++}"
    nodes.append("$name [label=\"Function: ${n.identifier.text}, return: ${n.returnType.identifier?.text}\"]\n")
    edges.append("cu->$name\n")
    for (p in n.parameters) printAstNode(name, p, nodes, edges)

    var last = name
    for (s in n.body) {
        last = printAstNode(last, s, nodes, edges)
    }

    return name
}
