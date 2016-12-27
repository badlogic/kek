package kek

import kek.compiler.*
import kek.runtime.*
import org.junit.Test

class CompilerTest {
    @Test
    fun testCompiler() {
        val compilerState = kek.compiler.compile(listOf(
                Source("kek/src/test/resources/simple.kek", String(ParserTest::class.java.getResourceAsStream("/simple.kek").readBytes())),
                Source("kek/src/test/resources/math.kek", String(ParserTest::class.java.getResourceAsStream("/math.kek").readBytes()))
        ))

        for (m in compilerState.modules.values) {
            print(printModule(m))
        }
    }
}