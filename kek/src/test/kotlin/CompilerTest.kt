package kek

import org.junit.Test

class CompilerTest {
    @Test
    fun testCompiler() {
        val source = String(ParserTest::class.java.getResourceAsStream("/simple.kek").readBytes());
        val compilerState = compile(listOf(source))
    }
}