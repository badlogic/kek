package kek

data class CompilerState(val compilationUnits:MutableList<CompilationUnit> = mutableListOf<CompilationUnit>())

fun compile(sources: List<CharSequence>) : CompilerState {
    val state = CompilerState()

    for (source in sources) {
        state.compilationUnits.add(parse(source))
    }

    return state
}
