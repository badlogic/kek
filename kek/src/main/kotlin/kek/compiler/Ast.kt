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

class StructureNode(val name: Token, val fields: List<VariableDeclarationNode>, val functions: List<FunctionNode>, firstToken: Token, lastToken: Token) : AstNode(firstToken, lastToken) {
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

class BlockNode(val statements: List<StatementNode>, firstToken: Token, lastToken: Token) : AstNode(firstToken, lastToken)

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
    fun pushLoop(n: AstNode)
    fun popLoop(n: AstNode)
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
    override fun pushLoop(n: AstNode) {
    }

    override fun popLoop(n: AstNode) {
    }

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

fun traverseAst(ast: AstNode, visitor: AstVisitor, include: Set<Class<out AstNode>> = emptySet()) {
    if (!include.isEmpty() and !include.contains(ast.javaClass)) return
    when (ast) {
        is CompilationUnitNode -> {
            visitor.pushScope(ast)
            visitor.namespace(ast.module)

            for (s in ast.structs) {
                traverseAst(s, visitor, include)
            }

            for (f in ast.functions) {
                traverseAst(f, visitor, include)
            }

            visitor.compilationUnit(ast)
            visitor.popScope(ast)
        }
        is StructureNode -> {
            visitor.pushScope(ast)
            // Note: the visitor is responsible for traversing the methods!
            visitor.structure(ast)
            visitor.popScope(ast)
        }
        is FunctionNode -> {
            visitor.pushScope(ast)
            traverseAst(ast.body, visitor, include)
            visitor.function(ast)
            visitor.popScope(ast)
        }
        is BlockNode -> {
            visitor.pushScope(ast)
            for (s in ast.statements) traverseAst(s, visitor, include)
            visitor.popScope(ast)
        }
        is VariableDeclarationNode -> {
            if (ast.initializer != null) traverseAst(ast.initializer, visitor, include)
            visitor.variableDeclaration(ast)
        }
        is ReturnNode -> {
            if (ast.expression != null) traverseAst(ast.expression, visitor, include)
            visitor.returnStatement(ast)
        }
        is IfNode -> {
            traverseAst(ast.condition, visitor, include)
            traverseAst(ast.trueBody, visitor, include)
            for (e in ast.elseIfs) traverseAst(e, visitor, include)
            traverseAst(ast.falseBody, visitor, include)
            visitor.ifStatement(ast)
        }
        is ForNode -> {
            visitor.pushScope(ast)
            visitor.pushLoop(ast)
            for (s in ast.initializer) traverseAst(s, visitor, include)
            traverseAst(ast.condition, visitor, include)
            for (s in ast.increment) traverseAst(s, visitor, include)
            traverseAst(ast.body, visitor, include)
            visitor.forStatement(ast)
            visitor.popLoop(ast)
            visitor.popScope(ast)
        }
        is WhileNode -> {
            visitor.pushLoop(ast)
            traverseAst(ast.condition, visitor, include)
            traverseAst(ast.body, visitor, include)
            visitor.whileStatement(ast)
            visitor.popLoop(ast)
        }
        is DoNode -> {
            visitor.pushLoop(ast)
            traverseAst(ast.body, visitor, include)
            traverseAst(ast.condition, visitor, include)
            visitor.doStatement(ast)
            visitor.popLoop(ast)
        }
        is BreakNode -> {
            visitor.breakStatement(ast)
        }
        is ContinueNode -> {
            visitor.continueStatement(ast)
        }
        is UnaryOperatorNode -> {
            traverseAst(ast.expr, visitor, include)
            visitor.unaryOperator(ast)
        }
        is BinaryOperatorNode -> {
            traverseAst(ast.left, visitor, include)
            traverseAst(ast.right, visitor, include)
            visitor.binaryOperator(ast)
        }
        is TernaryOperatorNode -> {
            traverseAst(ast.left, visitor, include)
            traverseAst(ast.middle, visitor, include)
            traverseAst(ast.right, visitor, include)
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
            traverseAst(ast.index, visitor, include)
            traverseAst(ast.array, visitor, include)
            visitor.arrayAccess(ast)
        }
        is FieldAccessNode -> {
            traverseAst(ast.base, visitor, include)
            visitor.fieldAccess(ast)
        }
        is FunctionCallNode -> {
            // Note: the functionCall visitor is responsible for resolving the function
            // we don't traverse the function expression here
            for (arg in ast.arguments) traverseAst(arg, visitor, include)
            visitor.functionCall(ast)
        }
        is ParenthesisNode -> {
            traverseAst(ast.expr, visitor, include)
            visitor.parenthesis(ast)
        }
        else -> throw Exception("Unhandled AST node")
    }
}