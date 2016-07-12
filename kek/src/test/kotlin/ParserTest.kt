package kek

import org.junit.Test

class ParserTest {
    @Test
    fun testParser() {
        val source = String(ParserTest::class.java.getResourceAsStream("/test.kek").readBytes());
        parse(source);
    }
}
