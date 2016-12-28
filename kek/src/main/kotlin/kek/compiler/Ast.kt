package kek.compiler

import kek.runtime.Source

abstract class AstNode(val firstToken: Token, val lastToken: Token) {
    val annotations = mutableMapOf<Class<Any>, Any>()

    @Suppress("UNCHECKED_CAST")
    fun <T> getAnnotation(cls: Class<T>): T {
        return annotations[cls as Class<Any>] as T
    }

    @Suppress("UNCHECKED_CAST")
    fun <T> setAnnotation(annotation: T, cls: Class<T>) {
        annotations[cls as Class<Any>] = annotation as Any
    }
}

class CompilationUnitNode(val source: Source, val module: String = "", val imports: List<ImportNode>, val functions: List<FunctionNode>, val structs: List<StructureNode>, firstToken: Token, lastToken: Token) : AstNode(firstToken, lastToken) {
}

class ImportNode(val importName: String, firstToken: Token, lastToken: Token) : AstNode(firstToken, lastToken) {
}

class StructureNode(val name: Token, val fields: List<VariableDeclarationNode>, firstToken: Token, lastToken: Token) : AstNode(firstToken, lastToken) {
}

class FunctionNode(val name: Token, val parameters: List<ParameterNode>, val returnType: TypeNode, val body: BlockNode, val extern: Boolean, firstToken: Token, lastToken: Token) : AstNode(firstToken, lastToken) {
}

class ParameterNode(val name: Token, val type: TypeNode, firstToken: Token, lastToken: Token) : AstNode(firstToken, lastToken) {
}

open class TypeNode(val name: List<Token>, val isArray: Boolean, val isOptional: Boolean, firstToken: Token, lastToken: Token) : AstNode(firstToken, lastToken) {

    fun fullyQualfiedName(): String {
        val buffer = StringBuffer()
        for (i in name.indices) {
            buffer.append(name[i].text)
            if (i < name.size - 1) buffer.append(".")
        }
        return buffer.toString()
    }
}

class NoTypeNode(firstToken: Token, lastToken: Token) : TypeNode(emptyList<Token>(), false, false, firstToken, lastToken)

class BlockNode(val statements: List<StatementNode>, firstToken: Token, lastToken: Token): AstNode(firstToken, lastToken)

abstract class StatementNode(firstToken: Token, lastToken: Token) : AstNode(firstToken, lastToken)

class VariableDeclarationNode(val name: Token, val type: TypeNode, val initializer: ExpressionNode?, firstToken: Token, lastToken: Token) : StatementNode(firstToken, lastToken)

class ReturnNode(val expression: ExpressionNode?, firstToken: Token, lastToken: Token) : StatementNode(firstToken, lastToken)

class IfNode(val condition: ExpressionNode, val trueBody: BlockNode, val elseIfs: List<IfNode>, val falseBody: BlockNode, firstToken: Token, lastToken: Token) : StatementNode(firstToken, lastToken)

class ForNode(val initializer: List<VariableDeclarationNode>, val condition: ExpressionNode, val increment: List<ExpressionNode>, val body: BlockNode, firstToken: Token, lastToken: Token) : StatementNode(firstToken, lastToken)

class WhileNode(val condition: ExpressionNode, val body: BlockNode, firstToken: Token, lastToken: Token) : StatementNode(firstToken, lastToken)

class DoNode(val condition: ExpressionNode, val body: BlockNode, firstToken: Token, lastToken: Token) : StatementNode(firstToken, lastToken)

class BreakNode(firstToken: Token, lastToken: Token) : StatementNode(firstToken, lastToken)

class ContinueNode(firstToken: Token, lastToken: Token) : StatementNode(firstToken, lastToken)

abstract class ExpressionNode(firstToken: Token, lastToken: Token) : StatementNode(firstToken, lastToken)

class UnaryOperatorNode(val opType: Token, val expr: ExpressionNode, firstToken: Token, lastToken: Token) : ExpressionNode(firstToken, lastToken)

class BinaryOperatorNode(val opType: Token, val left: ExpressionNode, val right: ExpressionNode, firstToken: Token, lastToken: Token) : ExpressionNode(firstToken, lastToken)

class TernaryOperatorNode(val left: ExpressionNode, val middle: ExpressionNode, val right: ExpressionNode, firstToken: Token, lastToken: Token) : ExpressionNode(firstToken, lastToken)

class CharacterLiteralNode(val literal: Token, firstToken: Token, lastToken: Token) : ExpressionNode(firstToken, lastToken)

class StringLiteralNode(val literal: Token, firstToken: Token, lastToken: Token) : ExpressionNode(firstToken, lastToken)

class NumberLiteralNode(val literal: Token, firstToken: Token, lastToken: Token) : ExpressionNode(firstToken, lastToken)

class BooleanLiteralNode(val literal: Token, firstToken: Token, lastToken: Token) : ExpressionNode(firstToken, lastToken)

class NullLiteralNode(firstToken: Token, lastToken: Token) : ExpressionNode(firstToken, lastToken)

class VariableAccessNode(val varName: Token, firstToken: Token, lastToken: Token) : ExpressionNode(firstToken, lastToken)

class ArrayAccessNode(val array: ExpressionNode, val index: ExpressionNode, firstToken: Token, lastToken: Token) : ExpressionNode(firstToken, lastToken)

class FieldAccessNode(val base: ExpressionNode, val varName: Token, firstToken: Token, lastToken: Token) : ExpressionNode(firstToken, lastToken)

class FunctionCallNode(val function: ExpressionNode, val arguments: List<ExpressionNode>, firstToken: Token, lastToken: Token) : ExpressionNode(firstToken, lastToken)

class ParenthesisNode(val expr: ExpressionNode, firstToken: Token, lastToken: Token) : ExpressionNode(firstToken, lastToken)

interface AstVisitor {
    fun pushScope(n: AstNode)
    fun popScope(n: AstNode)
    fun namespace(namespace: String)
    fun compilationUnit(n: CompilationUnitNode)
    fun structure(n: StructureNode)
    fun function(n: FunctionNode)
    fun block(n: BlockNode)
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
    fun nullLiteral(n: NullLiteralNode)
    fun variableAccess(n: VariableAccessNode)
    fun arrayAccess(n: ArrayAccessNode)
    fun fieldAccess(n: FieldAccessNode)
    fun functionCall(n: FunctionCallNode)
    fun parenthesis(n: ParenthesisNode)
}

abstract class AstVisitorAdapter : AstVisitor {
    override fun pushScope(n: AstNode) {
    }

    override fun popScope(n: AstNode) {
    }

    override fun namespace(namespace: String) {
    }

    override fun compilationUnit(n: CompilationUnitNode) {
    }

    override fun structure(n: StructureNode) {
    }

    override fun function(n: FunctionNode) {
    }

    override fun block(n: BlockNode) {
    }

    override fun variableDeclaration(n: VariableDeclarationNode) {
    }

    override fun returnStatement(n: ReturnNode) {
    }

    override fun ifStatement(n: IfNode) {
    }

    override fun forStatement(n: ForNode) {
    }

    override fun whileStatement(n: WhileNode) {
    }

    override fun doStatement(n: DoNode) {
    }

    override fun breakStatement(n: BreakNode) {
    }

    override fun continueStatement(n: ContinueNode) {
    }

    override fun unaryOperator(n: UnaryOperatorNode) {
    }

    override fun binaryOperator(n: BinaryOperatorNode) {
    }

    override fun ternaryOperator(n: TernaryOperatorNode) {
    }

    override fun charLiteral(n: CharacterLiteralNode) {
    }

    override fun stringLiteral(n: StringLiteralNode) {
    }

    override fun numberLiteral(n: NumberLiteralNode) {
    }

    override fun booleanLiteral(n: BooleanLiteralNode) {
    }

    override fun nullLiteral(n: NullLiteralNode) {
    }

    override fun variableAccess(n: VariableAccessNode) {
    }

    override fun arrayAccess(n: ArrayAccessNode) {
    }

    override fun fieldAccess(n: FieldAccessNode) {
    }

    override fun functionCall(n: FunctionCallNode) {
    }

    override fun parenthesis(n: ParenthesisNode) {
    }
}

fun traverseAstDepthFirst(ast: AstNode, visitor: AstVisitor, include: Set<Class<out AstNode>> = emptySet()) {
    if (!include.isEmpty() and !include.contains(ast.javaClass)) return
    when (ast) {
        is CompilationUnitNode -> {
            visitor.pushScope(ast)
            visitor.namespace(ast.module)

            for (s in ast.structs) {
                traverseAstDepthFirst(s, visitor, include)
            }

            for (f in ast.functions) {
                traverseAstDepthFirst(f, visitor, include)
            }

            visitor.compilationUnit(ast)
            visitor.popScope(ast)
        }
        is StructureNode -> {
            visitor.pushScope(ast)
            visitor.structure(ast)
            visitor.popScope(ast)
        }
        is FunctionNode -> {
            visitor.pushScope(ast)
            traverseAstDepthFirst(ast.body, visitor, include)
            visitor.function(ast)
            visitor.popScope(ast)
        }
        is BlockNode -> {
            visitor.pushScope(ast)
            for (s in ast.statements) traverseAstDepthFirst(s, visitor, include)
            visitor.popScope(ast)
        }
        is VariableDeclarationNode -> {
            visitor.variableDeclaration(ast)
        }
        is ReturnNode -> {
            visitor.returnStatement(ast)
        }
        is IfNode -> {
            traverseAstDepthFirst(ast.condition, visitor, include)
            traverseAstDepthFirst(ast.trueBody, visitor, include)
            for (e in ast.elseIfs) traverseAstDepthFirst(e, visitor, include)
            traverseAstDepthFirst(ast.falseBody, visitor, include)
            visitor.ifStatement(ast)
        }
        is ForNode -> {
            visitor.pushScope(ast)
            for (s in ast.initializer) traverseAstDepthFirst(s, visitor, include)
            traverseAstDepthFirst(ast.condition, visitor, include)
            for (s in ast.increment) traverseAstDepthFirst(s, visitor, include)
            traverseAstDepthFirst(ast.body, visitor, include)
            visitor.forStatement(ast)
            visitor.popScope(ast)
        }
        is WhileNode -> {
            traverseAstDepthFirst(ast.condition, visitor, include)
            traverseAstDepthFirst(ast.body, visitor, include)
            visitor.whileStatement(ast)
        }
        is DoNode -> {
            traverseAstDepthFirst(ast.body, visitor, include)
            traverseAstDepthFirst(ast.condition, visitor, include)
            visitor.doStatement(ast)
        }
        is BreakNode -> {
            visitor.breakStatement(ast)
        }
        is ContinueNode -> {
            visitor.continueStatement(ast)
        }
        is UnaryOperatorNode -> {
            traverseAstDepthFirst(ast.expr, visitor, include)
            visitor.unaryOperator(ast)
        }
        is BinaryOperatorNode -> {
            traverseAstDepthFirst(ast.left, visitor, include)
            traverseAstDepthFirst(ast.right, visitor, include)
            visitor.binaryOperator(ast)
        }
        is TernaryOperatorNode -> {
            traverseAstDepthFirst(ast.left, visitor, include)
            traverseAstDepthFirst(ast.middle, visitor, include)
            traverseAstDepthFirst(ast.right, visitor, include)
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
        is NullLiteralNode -> {
            visitor.nullLiteral(ast)
        }
        is VariableAccessNode -> {
            visitor.variableAccess(ast)
        }
        is ArrayAccessNode -> {
            traverseAstDepthFirst(ast.index, visitor, include)
            traverseAstDepthFirst(ast.array, visitor, include)
            visitor.arrayAccess(ast)
        }
        is FieldAccessNode -> {
            traverseAstDepthFirst(ast.base, visitor, include)
            visitor.fieldAccess(ast)
        }
        is FunctionCallNode -> {
            traverseAstDepthFirst(ast.function, visitor, include)
            visitor.functionCall(ast)
        }
        is ParenthesisNode -> {
            traverseAstDepthFirst(ast.expr, visitor, include)
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
    when(n) {
        is StructureNode -> return printStructureDefinition(n, nodes, edges)
        is FunctionNode -> return printFunctionDefinition(p, n, nodes, edges)
        is BlockNode -> return printBlock(p, n, nodes, edges)
        is ParameterNode -> return printParameter(p, n, nodes, edges)
        is UnaryOperatorNode -> return printUnaryOperator(p, n, nodes, edges)
        is BinaryOperatorNode -> return printBinaryOperator(p, n, nodes, edges)
        is CharacterLiteralNode -> return printCharacterLiteral(p, n, nodes, edges)
        is StringLiteralNode -> return printStringLiteral(p, n, nodes, edges)
        is NumberLiteralNode -> return printNumberLiteral(p, n, nodes, edges)
        is BooleanLiteralNode -> return printBooleanLiteral(p, n, nodes, edges)
        is NullLiteralNode -> return printNullLiteral(p, n, nodes, edges)
        is VariableAccessNode -> return printVariableAccess(p, n, nodes, edges)
        is ArrayAccessNode -> return printArrayAccess(p, n, nodes, edges)
        is FieldAccessNode -> return printFieldAccess(p, n, nodes, edges)
        is FunctionCallNode -> return printFunctionCall(p, n, nodes, edges)
        is VariableDeclarationNode -> return printVariableDeclaration(p, n, nodes, edges)
        is IfNode -> return printIfStatement(p, n, nodes, edges)
        is ForNode -> return printForStatement(p, n, nodes, edges)
        is WhileNode -> return printWhileStatement(p, n, nodes, edges)
        is DoNode -> return printDoStatement(p, n, nodes, edges)
        is ReturnNode -> return printReturnStatement(p, n, nodes, edges)
        is BreakNode -> return printBreakStatement(p, nodes, edges)
        is ContinueNode -> return printContinueStatement(p, nodes, edges)
        is TernaryOperatorNode -> return printTernaryOperator(p, n, nodes, edges)
        is ParenthesisNode -> return printParanthesis(p, n, nodes, edges)
        is ImportNode -> return printImport(p, n, nodes, edges)
        else -> throw RuntimeException("Unknown AST node $n")
    }
}

fun  printBlock(p: String, n: BlockNode, nodes: StringBuffer, edges: StringBuffer): String {
    return printAstNodeList(p, n.statements, nodes, edges)
}

fun printImport(p: String, n: ImportNode, nodes: StringBuffer, edges: StringBuffer): String {
    val name = "b${i++}"
    nodes.append("$name [label=\"ImportNode ${n.importName}\"]\n")
    edges.append("$p->$name\n")
    return name
}

fun printParanthesis(p: String, n: ParenthesisNode, nodes: StringBuffer, edges: StringBuffer): String {
    val name = "b${i++}"
    nodes.append("$name [label=\"()\"]\n")
    edges.append("$p->$name\n")
    printAstNode(name, n.expr, nodes, edges)
    return name
}

fun printContinueStatement(p: String, nodes: StringBuffer, edges: StringBuffer): String {
    val name = "b${i++}"
    nodes.append("$name [label=\"Continue\"]\n")
    edges.append("$p->$name\n")
    return name
}

fun printBreakStatement(p: String, nodes: StringBuffer, edges: StringBuffer): String {
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
    printAstNode(name, n.body, nodes, edges)
    return name
}

fun printDoStatement(p: String, n: DoNode, nodes: StringBuffer, edges: StringBuffer): String {
    val name = "b${i++}"
    nodes.append("$name [label=\"Do\"]\n")
    edges.append("$p->$name\n")
    printAstNode(name, n.condition, nodes, edges)
    printAstNode(name, n.body, nodes, edges)
    return name
}

fun printForStatement(p: String, n: ForNode, nodes: StringBuffer, edges: StringBuffer): String {
    val name = "b${i++}"
    nodes.append("$name [label=\"For\"]\n")
    edges.append("$p->$name\n")
    printAstNodeList(name, n.initializer, nodes, edges)
    printAstNode(name, n.condition, nodes, edges)
    printAstNodeList(name, n.increment, nodes, edges)
    printAstNode(name, n.body, nodes, edges)
    return name
}

fun printReturnStatement(p: String, n: ReturnNode, nodes: StringBuffer, edges: StringBuffer): String {
    val name = "b${i++}"
    nodes.append("$name [label=\"Return\"]\n")
    edges.append("$p->$name\n")
    if (n.expression != null) printAstNode(name, n.expression, nodes, edges)
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
    printAstNode(name, n.trueBody, nodes, edges)
    printAstNodeList(name, n.elseIfs, nodes, edges)
    printAstNode(name, n.falseBody, nodes, edges)
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
    if (n.initializer != null) printAstNode(name, n.initializer, nodes, edges)
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

fun printNullLiteral(p: String, n: NullLiteralNode, nodes: StringBuffer, edges: StringBuffer): String {
    val name = "s${i++}"
    nodes.append("$name [label=\"Null\"]\n")
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

fun printFunctionDefinition(p: String, n: FunctionNode, nodes: StringBuffer, edges: StringBuffer): String {
    val name = "f${i++}"
    nodes.append("$name [label=\"Function: ${n.name.text}, return: ${n.returnType.fullyQualfiedName()}\"]\n")
    edges.append("$p->$name\n")
    for (p in n.parameters) printAstNode(name, p, nodes, edges)

    var last = name
    printAstNode(name, n.body, nodes, edges)

    return name
}
