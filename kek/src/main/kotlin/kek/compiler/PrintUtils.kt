package kek.compiler

import kek.runtime.Module

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

fun printModule(module: Module): String {
    val buffer = StringBuffer()

    buffer.appendln("Module ${module.name}")
    buffer.appendln("\tImports")
    for (i in module.imports())
        buffer.appendln("\t\t${i}")
    buffer.appendln("\tPrimitive Types")
    for (t in module.primitiveTypes().values)
        buffer.appendln("\t\t${t}")
    buffer.appendln("\tStructures")
    for (s in module.structures().values) {
        buffer.appendln("\t\t${s.name}")
        for (f in s.fields) {
            buffer.appendln("\t\t\t${f.name}: ${f.type}")
        }
        buffer.appendln()
    }
    buffer.appendln("\tFunctions")
    for (fl in module.functions().values)
        for (f in fl) {
            buffer.appendln("\t\t${f.name}")
            for (p in f.parameters) {
                buffer.appendln("\t\t\t${p.name}: ${p.type}")
            }
            buffer.appendln("\t\t\treturn: ${f.returnType}")
        }
    return buffer.toString()
}