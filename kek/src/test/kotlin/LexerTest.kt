package kek

import org.junit.Test
import org.junit.Assert.*;

class LexerTest {
    @Test
    fun testEmptySource() {
        var tokens = tokenize("")
        assertEquals(1, tokens.size)
        assertEquals(Token(TokenType.EOF, 0, 1, 1, ""), tokens[0])
    }

    @Test
    fun testNewLine() {
        var tokens = tokenize("    \n   \n  ");
        assertEquals(1, tokens.size)
        assertEquals(Token(TokenType.EOF, 11, 3, 3, ""), tokens[0])
    }

    @Test
    fun testLineComment() {
        var tokens = tokenize("    \n// this is a test")
        assertEquals(1, tokens.size)
        assertEquals(Token(TokenType.EOF, 22, 2, 18, ""), tokens[0])
    }

    @Test
    fun testBlockComment() {
        var tokens = tokenize("    \n/* this is a \n  test */\n")
        assertEquals(1, tokens.size)
        assertEquals(Token(TokenType.EOF, 29, 4, 1, ""), tokens[0])
    }

    @Test
    fun testUnknownToken() {
        try {
            var tokens = tokenize("// this is a test\n /* this is \n another test */  .")
            assertTrue(false)
        } catch (e: CompilerException) {
            // expected
        }
    }

    @Test
    fun testKeywords() {
        var tokens = tokenize ("( ) [ ] : , + - * / if then else def val var")
    }

    @Test
    fun testNumLiterals() {

    }
}