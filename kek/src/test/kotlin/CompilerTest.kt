package kek

import kek.compiler.*
import kek.runtime.*
import org.junit.Test

class CompilerTest {
    @Test
    fun testCompiler() {
        val source = String(ParserTest::class.java.getResourceAsStream("/simple.kek").readBytes());
        val compilerState = kek.compiler.compile(listOf(Source("kek/src/test/resources/simple.kek", source)))

        for (m in compilerState.modules.values) {
            print(printModule(m))
        }
    }
}