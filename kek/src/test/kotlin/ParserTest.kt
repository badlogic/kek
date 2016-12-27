package kek

import org.junit.Test
import kek.compiler.*
import kek.runtime.*

class ParserTest {
    @Test
    fun testParser() {
        val source = String(ParserTest::class.java.getResourceAsStream("/test.kek").readBytes());
        val cu = parse(Source("kek/src/test/resources/test.kek", source));
        print(printAst(cu));
    }
}
