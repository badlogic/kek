package kek

class CompilerException (val source: String,
                         val msg: String,
                         val line: Int,
                         val lineStart: Int,
                         val lineEnd: Int) : RuntimeException(msg) {

    override fun toString(): String {
        val lines = source.split('\n');
        if (!lines.isEmpty()) {
            return "Error line ${line}, column ${lineStart}: ${msg} ${lines[line - 1].substring(lineStart - 1)}"
        } else {
            return "Error: ${msg}"
        }
    }
}
