package kek.compiler

import kek.runtime.*

abstract class AstNode (val firstToken: Token, val lastToken: Token){
    val annotations = mutableMapOf<Class<Any>, Any> ()

    fun <T> getAnnotation(cls:Class<T>): T {
        return annotations[cls as Class<Any>] as T
    }

    fun <T> setAnnotation(annotation: T, cls:Class<T>) {
        annotations[cls as Class<Any>] = annotation as Any
    }
}

class CompilationUnitNode(val source: Source, val module: String = "", val imports: List<ImportNode>, val functions: List<FunctionNode>, val structs: List<StructureNode>, firstToken: Token, lastToken: Token) : AstNode(firstToken, lastToken) {
}

class ImportNode(val importName: String, firstToken: Token, lastToken: Token): AstNode(firstToken, lastToken) {
}

class StructureNode(val name: Token, val fields: List<VariableDeclarationNode>, firstToken: Token, lastToken: Token) : AstNode(firstToken, lastToken) {
}

class FunctionNode(val name: Token, val parameters: List<ParameterNode>, val returnType: TypeNode, val body: List<StatementNode>, firstToken: Token, lastToken: Token) : AstNode(firstToken, lastToken) {
}

class ParameterNode(val name: Token, val type: TypeNode, firstToken: Token, lastToken: Token) : AstNode(firstToken, lastToken) {
}

open class TypeNode(val name: List<Token>, val isArray:Boolean, val isOptional:Boolean, firstToken: Token, lastToken: Token) : AstNode(firstToken, lastToken) {

    fun fullyQualfiedName():String {
        val buffer = StringBuffer()
        for (i in name.indices) {
            buffer.append(name[i].text)
            if (i < name.size - 1) buffer.append(".")
        }
        return buffer.toString()
    }
}

class NoTypeNode(firstToken: Token, lastToken: Token) : TypeNode(emptyList<Token>(), false, false, firstToken, lastToken) {
}

abstract class StatementNode(firstToken: Token, lastToken: Token) : AstNode(firstToken, lastToken) {
}

class VariableDeclarationNode(val name: Token, val type: TypeNode, val initializer: ExpressionNode, firstToken: Token, lastToken: Token) : StatementNode(firstToken, lastToken) {
}

class ReturnNode(val expression: ExpressionNode, firstToken: Token, lastToken: Token) : StatementNode(firstToken, lastToken) {
}

class IfNode(val condition: ExpressionNode, val trueBody: List<StatementNode>, val elseIfs: List<IfNode>, val falseBody: List<StatementNode>, firstToken: Token, lastToken: Token) : StatementNode(firstToken, lastToken) {
}

class ForNode(val initializer: List<VariableDeclarationNode>, val condition: ExpressionNode, val increment: List<ExpressionNode>, val body: List<StatementNode>, firstToken: Token, lastToken: Token) : StatementNode(firstToken, lastToken) {
}

class WhileNode(val condition: ExpressionNode, val body: List<StatementNode>, firstToken: Token, lastToken: Token) : StatementNode(firstToken, lastToken) {
}

class DoNode(val condition: ExpressionNode, val body: List<StatementNode>, firstToken: Token, lastToken: Token) : StatementNode(firstToken, lastToken) {
}

class BreakNode(firstToken: Token, lastToken: Token) : StatementNode(firstToken, lastToken) {
}

class ContinueNode(firstToken: Token, lastToken: Token) : StatementNode(firstToken, lastToken) {
}

abstract class ExpressionNode(firstToken: Token, lastToken: Token) : StatementNode(firstToken, lastToken) {
}

class EmptyExpressionNode(firstToken: Token, lastToken: Token) : ExpressionNode(firstToken, lastToken) {
}

class UnaryOperatorNode(val opType: Token, val expr: ExpressionNode, firstToken: Token, lastToken: Token) : ExpressionNode(firstToken, lastToken) {
}

class BinaryOperatorNode(val opType: Token, val left: ExpressionNode, val right: ExpressionNode, firstToken: Token, lastToken: Token) : ExpressionNode(firstToken, lastToken) {
}

class TernaryOperatorNode(val left: ExpressionNode, val middle: ExpressionNode, val right: ExpressionNode, firstToken: Token, lastToken: Token) : ExpressionNode(firstToken, lastToken) {
}

class CharacterLiteralNode(val literal: Token, firstToken: Token, lastToken: Token) : ExpressionNode(firstToken, lastToken) {
}

class StringLiteralNode(val literal: Token, firstToken: Token, lastToken: Token) : ExpressionNode(firstToken, lastToken) {
}

class NumberLiteralNode(val literal: Token, firstToken: Token, lastToken: Token) : ExpressionNode(firstToken, lastToken) {
}

class BooleanLiteralNode(val literal: Token, firstToken: Token, lastToken: Token) : ExpressionNode(firstToken, lastToken) {
}

class VariableAccessNode(val varName: Token, firstToken: Token, lastToken: Token) : ExpressionNode(firstToken, lastToken) {
}

class ArrayAccessNode(val array: ExpressionNode, val index: ExpressionNode, firstToken: Token, lastToken: Token) : ExpressionNode(firstToken, lastToken) {
}

class FieldAccessNode(val base: ExpressionNode, val varName: Token, firstToken: Token, lastToken: Token) : ExpressionNode(firstToken, lastToken) {
}

class FunctionCallNode(val function: ExpressionNode, val arguments: List<ExpressionNode>, firstToken: Token, lastToken: Token) : ExpressionNode(firstToken, lastToken) {
}

// needed for formatting
class ParenthesisNode(val expr: ExpressionNode, firstToken: Token, lastToken: Token) : ExpressionNode(firstToken, lastToken) {
}

interface AstVisitor {
    fun namespace(namespace: String)
    fun compilationUnit(n: CompilationUnitNode)
    fun structure(n: StructureNode)
    fun function(n: FunctionNode)
    fun variableDeclaration(n: VariableDeclarationNode)
    fun returnStatement(n: ReturnNode)
    fun ifStatement(n: IfNode)
    fun forStatement(n: ForNode)
    fun whileStatement(n: WhileNode)
    fun doStatement(n: DoNode)
    fun breakStatement(n: BreakNode)
    fun continueStatement(n: ContinueNode)
    fun unaryOperator(n: UnaryOperatorNode)
    fun binaryOperator(n: BinaryOperatorNode)
    fun ternaryOperator(n: TernaryOperatorNode)
    fun charLiteral(n: CharacterLiteralNode)
    fun stringLiteral(n: StringLiteralNode)
    fun numberLiteral(n: NumberLiteralNode)
    fun booleanLiteral(n: BooleanLiteralNode)
    fun variableAccess(n: VariableAccessNode)
    fun arrayAccess(n: ArrayAccessNode)
    fun fieldAccess(n: FieldAccessNode)
    fun functionCall(n: FunctionCallNode)
    fun parenthesis(n: ParenthesisNode)
}

fun traverseAstDepthFirst(ast: AstNode, visitor: AstVisitor) {
    when(ast) {
        is CompilationUnitNode -> {
            visitor.namespace(ast.module)

            for (s in ast.structs) {
                traverseAstDepthFirst(s, visitor)
                visitor.structure(s)
            }

            for (f in ast.functions) {
                traverseAstDepthFirst(f, visitor)
            }

            visitor.compilationUnit(ast)
        }
        is StructureNode -> {
            visitor.structure(ast)
        }
        is FunctionNode -> {
            for (s in ast.body) traverseAstDepthFirst(s, visitor)
            visitor.function(ast)
        }
        is VariableDeclarationNode -> {
            visitor.variableDeclaration(ast)
        }
        is ReturnNode -> {
            visitor.returnStatement(ast)
        }
        is IfNode -> {
            traverseAstDepthFirst(ast.condition, visitor)
            for (s in ast.trueBody) traverseAstDepthFirst(s, visitor)
            for (e in ast.elseIfs) traverseAstDepthFirst(e, visitor)
            for (s in ast.falseBody) traverseAstDepthFirst(s, visitor)
            visitor.ifStatement(ast)
        }
        is ForNode -> {
            for (s in ast.initializer) traverseAstDepthFirst(s, visitor)
            traverseAstDepthFirst(ast.condition, visitor)
            for (s in ast.increment) traverseAstDepthFirst(s, visitor)
            for (s in ast.body) traverseAstDepthFirst(s, visitor)
            visitor.forStatement(ast)
        }
        is WhileNode -> {
            traverseAstDepthFirst(ast.condition, visitor)
            for (s in ast.body) traverseAstDepthFirst(s, visitor)
            visitor.whileStatement(ast)
        }
        is DoNode -> {
            for (s in ast.body) traverseAstDepthFirst(s, visitor)
            traverseAstDepthFirst(ast.condition, visitor)
            visitor.doStatement(ast)
        }
        is BreakNode -> {
            visitor.breakStatement(ast)
        }
        is ContinueNode -> {
            visitor.continueStatement(ast)
        }
        is UnaryOperatorNode -> {
            traverseAstDepthFirst(ast.expr, visitor)
            visitor.unaryOperator(ast)
        }
        is BinaryOperatorNode -> {
            traverseAstDepthFirst(ast.left, visitor)
            traverseAstDepthFirst(ast.right, visitor)
            visitor.binaryOperator(ast)
        }
        is TernaryOperatorNode -> {
            traverseAstDepthFirst(ast.left, visitor)
            traverseAstDepthFirst(ast.middle, visitor)
            traverseAstDepthFirst(ast.right, visitor)
            visitor.ternaryOperator(ast)
        }
        is CharacterLiteralNode -> {
            visitor.charLiteral(ast)
        }
        is StringLiteralNode -> {
            visitor.stringLiteral(ast)
        }
        is NumberLiteralNode -> {
            visitor.numberLiteral(ast)
        }
        is BooleanLiteralNode -> {
            visitor.booleanLiteral(ast)
        }
        is VariableAccessNode -> {
            visitor.variableAccess(ast)
        }
        is ArrayAccessNode -> {
            traverseAstDepthFirst(ast.index, visitor)
            traverseAstDepthFirst(ast.array, visitor)
            visitor.arrayAccess(ast)
        }
        is FieldAccessNode -> {
            traverseAstDepthFirst(ast.base, visitor)
            visitor.fieldAccess(ast)
        }
        is FunctionCallNode -> {
            traverseAstDepthFirst(ast.function, visitor)
            visitor.functionCall(ast)
        }
        is ParenthesisNode -> {
            traverseAstDepthFirst(ast.expr, visitor)
            visitor.parenthesis(ast)
        }
        else -> throw Exception("Unhandled AST node")
    }
}


var i = 0

fun printAst(cu: CompilationUnitNode): String {
    val buffer = StringBuffer()
    val nodes = StringBuffer()
    val edges = StringBuffer()

    buffer.append("digraph cu {\n")
    nodes.append("node[shape=box]\n")
    nodes.append("cu [label=\"Compilation Unit, source ${cu.source.location}\", shape=box]\n")
    nodes.append("ns [label=\"Namespace: ${cu.module}\"]\n")
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
    if (n is FunctionNode) return printFunctionDefinition(n, nodes, edges)
    else if (n is StructureNode) return printStructureDefinition(n, nodes, edges)
    else if (n is ParameterNode) return printParameter(p, n, nodes, edges)
    else if (n is UnaryOperatorNode) return printUnaryOperator(p, n, nodes, edges)
    else if (n is BinaryOperatorNode) return printBinaryOperator(p, n, nodes, edges)
    else if (n is CharacterLiteralNode) return printCharacterLiteral(p, n, nodes, edges)
    else if (n is StringLiteralNode) return printStringLiteral(p, n, nodes, edges)
    else if (n is NumberLiteralNode) return printNumberLiteral(p, n, nodes, edges)
    else if (n is BooleanLiteralNode) return printBooleanLiteral(p, n, nodes, edges)
    else if (n is VariableAccessNode) return printVariableAccess(p, n, nodes, edges)
    else if (n is ArrayAccessNode) return printArrayAccess(p, n, nodes, edges)
    else if (n is FieldAccessNode) return printFieldAccess(p, n, nodes, edges)
    else if (n is FunctionCallNode) return printFunctionCall(p, n, nodes, edges)
    else if (n is VariableDeclarationNode) return printVariableDeclaration(p, n, nodes, edges)
    else if (n is IfNode) return printIfStatement(p, n, nodes, edges)
    else if (n is ForNode) return printForStatement(p, n, nodes, edges)
    else if (n is WhileNode) return printWhileStatement(p, n, nodes, edges)
    else if (n is DoNode) return printDoStatement(p, n, nodes, edges)
    else if (n is ReturnNode) return printReturnStatement(p, n, nodes, edges)
    else if (n is BreakNode) return printBreakStatement(p, n, nodes, edges)
    else if (n is ContinueNode) return printContinueStatement(p, n, nodes, edges)
    else if (n is TernaryOperatorNode) return printTernaryOperator(p, n, nodes, edges)
    else if (n is EmptyExpressionNode) return "empty"
    else if (n is ParenthesisNode) return printParanthesis(p, n, nodes, edges)
    else if (n is ImportNode) return printImport(p, n, nodes, edges)
    else throw RuntimeException("Unknown AST node $n")
}

fun  printImport(p: String, n: ImportNode, nodes: StringBuffer, edges: StringBuffer): String {
    val name = "b${i++}"
    nodes.append("$name [label=\"ImportNode ${n.importName}\"]\n")
    edges.append("$p->$name\n")
    return name
}

fun  printParanthesis(p: String, n: ParenthesisNode, nodes: StringBuffer, edges: StringBuffer): String {
    val name = "b${i++}"
    nodes.append("$name [label=\"()\"]\n")
    edges.append("$p->$name\n")
    printAstNode(name, n.expr, nodes, edges)
    return name
}

fun printContinueStatement(p: String, n: ContinueNode, nodes: StringBuffer, edges: StringBuffer): String {
    val name = "b${i++}"
    nodes.append("$name [label=\"Continue\"]\n")
    edges.append("$p->$name\n")
    return name
}

fun printBreakStatement(p: String, n: BreakNode, nodes: StringBuffer, edges: StringBuffer): String {
    val name = "b${i++}"
    nodes.append("$name [label=\"Break\"]\n")
    edges.append("$p->$name\n")
    return name
}

fun printWhileStatement(p: String, n: WhileNode, nodes: StringBuffer, edges: StringBuffer): String {
    val name = "b${i++}"
    nodes.append("$name [label=\"While\"]\n")
    edges.append("$p->$name\n")
    printAstNode(name, n.condition, nodes, edges)
    printAstNodeList(name, n.body, nodes, edges)
    return name
}

fun printDoStatement(p: String, n: DoNode, nodes: StringBuffer, edges: StringBuffer): String {
    val name = "b${i++}"
    nodes.append("$name [label=\"Do\"]\n")
    edges.append("$p->$name\n")
    printAstNode(name, n.condition, nodes, edges)
    printAstNodeList(name, n.body, nodes, edges)
    return name
}

fun printForStatement(p: String, n: ForNode, nodes: StringBuffer, edges: StringBuffer): String {
    val name = "b${i++}"
    nodes.append("$name [label=\"For\"]\n")
    edges.append("$p->$name\n")
    printAstNodeList(name, n.initializer, nodes, edges)
    printAstNode(name, n.condition, nodes, edges)
    printAstNodeList(name, n.increment, nodes, edges)
    printAstNodeList(name, n.body, nodes, edges)
    return name
}

fun printReturnStatement(p: String, n: ReturnNode, nodes: StringBuffer, edges: StringBuffer): String {
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

fun printIfStatement(p: String, n: IfNode, nodes: StringBuffer, edges: StringBuffer): String {
    val name = "b${i++}"
    nodes.append("$name [label=\"If\"]\n")
    edges.append("$p->$name\n")
    printAstNode(name, n.condition, nodes, edges)
    printAstNodeList(name, n.trueBody, nodes, edges)
    printAstNodeList(name, n.elseIfs, nodes, edges)
    printAstNodeList(name, n.falseBody, nodes, edges)
    return name
}

fun printTernaryOperator(p: String, n: TernaryOperatorNode, nodes: StringBuffer, edges: StringBuffer): String {
    val name = "b${i++}"
    nodes.append("$name [label=\"Ternary Operator\"]\n")
    edges.append("$p->$name\n")
    printAstNode(name, n.left, nodes, edges)
    printAstNode(name, n.middle, nodes, edges)
    printAstNode(name, n.right, nodes, edges)
    return name
}

fun printVariableDeclaration(p: String, n: VariableDeclarationNode, nodes: StringBuffer, edges: StringBuffer): String {
    val name = "b${i++}"
    nodes.append("$name [label=\"Variable ${n.name.text}, ${n.type.fullyQualfiedName()}, optional: ${n.type.isOptional}, array: ${n.type.isArray}\"]\n")
    edges.append("$p->$name\n")
    printAstNode(name, n.initializer, nodes, edges)
    return name
}

fun printFunctionCall(p: String, n: FunctionCallNode, nodes: StringBuffer, edges: StringBuffer): String {
    val name = "b${i++}"
    nodes.append("$name [label=\"Call ()\"]\n")
    edges.append("$p->$name\n")
    printAstNode(name, n.function, nodes, edges)
    for (a in n.arguments) printAstNode(name, a, nodes, edges)
    return name
}

fun printFieldAccess(p: String, n: FieldAccessNode, nodes: StringBuffer, edges: StringBuffer): String {
    val name = "b${i++}"
    nodes.append("$name [label=\"Field Access .${n.varName.text}\"]\n")
    edges.append("$p->$name\n")
    printAstNode(name, n.base, nodes, edges)
    return name
}

fun printArrayAccess(p: String, n: ArrayAccessNode, nodes: StringBuffer, edges: StringBuffer): String {
    val name = "b${i++}"
    nodes.append("$name [label=\"Array Access []\"]\n")
    edges.append("$p->$name\n")
    printAstNode(name, n.array, nodes, edges)
    printAstNode(name, n.index, nodes, edges)
    return name
}

fun printVariableAccess(p: String, n: VariableAccessNode, nodes: StringBuffer, edges: StringBuffer): String {
    val name = "s${i++}"
    nodes.append("$name [label=\"Variable access '${n.varName.text}'\"]\n")
    edges.append("$p->$name\n")
    return name
}

fun printBooleanLiteral(p: String, n: BooleanLiteralNode, nodes: StringBuffer, edges: StringBuffer): String {
    val name = "s${i++}"
    nodes.append("$name [label=\"Boolean '${n.literal.text}'\"]\n")
    edges.append("$p->$name\n")
    return name
}

fun printNumberLiteral(p: String, n: NumberLiteralNode, nodes: StringBuffer, edges: StringBuffer): String {
    val name = "s${i++}"
    nodes.append("$name [label=\"Number '${n.literal.text}'\"]\n")
    edges.append("$p->$name\n")
    return name
}

fun printStringLiteral(p: String, n: StringLiteralNode, nodes: StringBuffer, edges: StringBuffer): String {
    val name = "s${i++}"
    nodes.append("$name [label=\"String '${n.literal.text}'\"]\n")
    edges.append("$p->$name\n")
    return name
}

fun printCharacterLiteral(p: String, n: CharacterLiteralNode, nodes: StringBuffer, edges: StringBuffer): String {
    val name = "s${i++}"
    nodes.append("$name [label=\"Character '${n.literal.text}'\"]\n")
    edges.append("$p->$name\n")
    return name
}

fun printBinaryOperator(p: String, n: BinaryOperatorNode, nodes: StringBuffer, edges: StringBuffer): String {
    val name = "b${i++}"
    nodes.append("$name [label=\"Binary ${n.opType.text}\"]\n")
    edges.append("$p->$name\n")
    printAstNode(name, n.left, nodes, edges)
    printAstNode(name, n.right, nodes, edges)
    return name
}

fun printUnaryOperator(p: String, n: UnaryOperatorNode, nodes: StringBuffer, edges: StringBuffer): String {
    val name = "n${i++}"
    nodes.append("$name [label=\"Unary ${n.opType.text}\"]\n")
    edges.append("$p->$name\n")
    printAstNode(name, n.expr, nodes, edges)
    return name
}

fun printParameter(p: String, n: ParameterNode, nodes: StringBuffer, edges: StringBuffer): String {
    val name = "p${i++}"
    nodes.append("$name [label=\"Param: ${n.name.text}, ${n.type.fullyQualfiedName()}\"]\n")
    edges.append("$p->$name\n")
    return name
}

fun printStructureDefinition(n: StructureNode, nodes: StringBuffer, edges: StringBuffer): String {
    val name = "s${i++}"
    nodes.append("$name [label=\"Structure: ${n.name.text}\"]\n")
    edges.append("cu->$name\n")
    for (f in n.fields) printAstNode(name, f, nodes, edges)
    return name
}

fun printFunctionDefinition(n: FunctionNode, nodes: StringBuffer, edges: StringBuffer): String {
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
