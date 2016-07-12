package kek

class CompilerException(val source: String,
                        val msg: String,
                        val line: Int,
                        val columnStart: Int,
                        val columnEnd: Int) : RuntimeException(msg) {

    override fun toString(): String {
        val lines = source.split('\n');
        if (!lines.isEmpty()) {
            return "Error line ${line}, column ${columnStart}: ${msg} ${lines[line - 1].substring(columnStart - 1)}"
        } else {
            return "Error: ${msg}"
        }
    }
}
