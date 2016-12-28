package kek

import kek.compiler.*
import kek.runtime.*
import org.junit.Test
import org.junit.Assert.*

class LexerTest {
    @Test
    fun testEmptySource() {
        var tokens = tokenize(Source("", ""))
        assertEquals(1, tokens.size)
        assertEquals(Token(TokenType.EOF, 0, 1, 1, ""), tokens[0])
    }

    @Test
    fun testNewLine() {
        var tokens = tokenize(Source("", "    \n   \n  "));
        assertEquals(1, tokens.size)
        assertEquals(Token(TokenType.EOF, 11, 3, 3, ""), tokens[0])
    }

    @Test
    fun testLineComment() {
        var tokens = tokenize(Source("", "    \n// this is a test"))
        assertEquals(1, tokens.size)
        assertEquals(Token(TokenType.EOF, 22, 2, 18, ""), tokens[0])
    }

    @Test
    fun testBlockComment() {
        var tokens = tokenize(Source("", "    \n/* this is a \n  test */\n"))
        assertEquals(1, tokens.size)
        assertEquals(Token(TokenType.EOF, 29, 4, 1, ""), tokens[0])
    }

    @Test
    fun testUnknownToken() {
        try {
            tokenize(Source("", "// this is a test\n /* this is \n another test */  §"))
            assertTrue(false)
        } catch (e: CompilerException) {
            // expected
        }
    }

    @Test
    fun testNumber() {
        var tokens = tokenize(Source("","0 1234 123.2 .234 0.123d 7123478943l"))
        assertEquals(8, tokens.size)
        assertEquals(TokenType.NUMBER, tokens[1].type)
        assertEquals("1234", tokens[1].text)
        assertEquals(TokenType.NUMBER, tokens[2].type)
        assertEquals("123.2", tokens[2].text)
        assertEquals(TokenType.DOT, tokens[3].type)
        assertEquals(".", tokens[3].text)
        assertEquals(TokenType.NUMBER, tokens[4].type)
        assertEquals("234", tokens[4].text)
        assertEquals(TokenType.NUMBER, tokens[5].type)
        assertEquals("0.123d", tokens[5].text)
        assertEquals(TokenType.NUMBER, tokens[6].type)
        assertEquals("7123478943l", tokens[6].text)
        assertEquals(TokenType.EOF, tokens[7].type)
    }

    @Test
    fun testIdentifier() {
        var tokens = tokenize(Source("", "a ab abC a123Bc a_c_d 1ab"))
        assertEquals(8, tokens.size)
        assertEquals(TokenType.IDENTIFIER, tokens[0].type)
        assertEquals("a", tokens[0].text)
        assertEquals(TokenType.IDENTIFIER, tokens[1].type)
        assertEquals("ab", tokens[1].text)
        assertEquals(TokenType.IDENTIFIER, tokens[2].type)
        assertEquals("abC", tokens[2].text)
        assertEquals(TokenType.IDENTIFIER, tokens[3].type)
        assertEquals("a123Bc", tokens[3].text)
        assertEquals(TokenType.IDENTIFIER, tokens[4].type)
        assertEquals("a_c_d", tokens[4].text)
        assertEquals(TokenType.NUMBER, tokens[5].type)
        assertEquals("1", tokens[5].text)
        assertEquals(TokenType.IDENTIFIER, tokens[6].type)
        assertEquals("ab", tokens[6].text)
        assertEquals(TokenType.EOF, tokens[7].type)
    }

    @Test
    fun testString() {
        val tokens = tokenize(Source("", """ "This is a string \\ \n \t \"" """))
        assertEquals(2, tokens.size)
        assertEquals(TokenType.STRING, tokens[0].type)
        assertEquals(""""This is a string \\ \n \t \""""", tokens[0].text)
        assertEquals(TokenType.EOF, tokens[1].type)

        try {
            tokenize(Source("", """ "this is a """));
            assertTrue(false)
        } catch(e: CompilerException) {
            // expected
        }

        try {
            tokenize(Source("", """ "\" """))
            assertTrue(false)
        } catch(e: CompilerException) {
            // expected
        }

        try {
            tokenize(Source("", """ "\z" """))
            assertTrue(false)
        } catch(e: CompilerException) {
            // expected
        }
    }

    @Test
    fun testCharacter() {
        val tokens = tokenize(Source("", """ 'a' '' '\'' '\n' '\t' """))
        assertEquals(6, tokens.size)
        assertEquals(TokenType.CHARACTER, tokens[0].type)
        assertEquals("'a'", tokens[0].text)
        assertEquals(TokenType.CHARACTER, tokens[1].type)
        assertEquals("''", tokens[1].text)
        assertEquals(TokenType.CHARACTER, tokens[2].type)
        assertEquals("""'\''""", tokens[2].text)
        assertEquals(TokenType.CHARACTER, tokens[3].type)
        assertEquals("""'\n'""", tokens[3].text)
        assertEquals(TokenType.CHARACTER, tokens[4].type)
        assertEquals("""'\t'""", tokens[4].text)
        assertEquals(TokenType.EOF, tokens[5].type)

        try {
            tokenize(Source("", " 'a"));
            assertTrue(false)
        } catch(e: CompilerException) {
            // expected
        }

        try {
            tokenize(Source("", """ '\' """"))
            assertTrue(false)
        } catch(e: CompilerException) {
            // expected
        }

        try {
            tokenize(Source("", """ '\z' """))
            assertTrue(false)
        } catch(e: CompilerException) {
            // expected
        }
    }

    @Test
    fun testKeywordIdentifierOverlap() {
        var tokens = tokenize (Source("", "and andThisIsAnIdentifier"))
        assertEquals(tokens.size, 3)
        assertEquals(TokenType.AND, tokens[0].type)
        assertEquals("and", tokens[0].text)
        assertEquals(TokenType.IDENTIFIER, tokens[1].type)
        assertEquals("andThisIsAnIdentifier", tokens[1].text)
    }

    @Test
    fun testKeywords() {
        val buffer = StringBuffer()
        val expected = mutableListOf<TokenType>()
        for (t in TokenType.values().filter { !it.keyword.isEmpty() }) {
            buffer.append(t.keyword)
            buffer.append(" ")
            expected.add(t)
        }
        expected.add(TokenType.EOF)
        var tokens = tokenize(Source("", buffer.toString()))
        var i = 0
        assertEquals(expected.size, tokens.size)
        for(e in expected) {
            val t = tokens.get(i++)
            assertEquals(e, t.type)
        }
    }
}