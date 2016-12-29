package kek

import kek.compiler.*
import kek.runtime.*
import org.junit.Test
import org.junit.Assert.*

class CompilerTest {
    val mathModule = Source("kek/src/test/resources/math.kek", String(ParserTest::class.java.getResourceAsStream("/math.kek").readBytes()))

    @Test
    fun testCompiler() {
        val compilerState = kek.compiler.compile(listOf(
                Source("kek/src/test/resources/simple.kek", String(ParserTest::class.java.getResourceAsStream("/simple.kek").readBytes())),
                mathModule
        ))
    }

    @Test
    fun test_EmptyProgram() {
        val state = compile(listOf(Source("test.kek", """



        """)))
    }

    @Test
    fun test_SimplestProgram() {
        val state = compile(listOf(
                Source("test.kek",
                        """
function main()
end
            """)
        ))

        val module = state.modules.values.first()
        assertEquals("", module.name)

        val func = module.functions()["main"]!!.first()
        assertEquals("main", func.name)
        assertEquals(0, func.parameters.size)
        assertEquals(VoidType, func.returnType)
    }

    @Test
    fun test_SimpleModule() {
        val state = compile(listOf(
                Source("test.kek", """
module my.mod.name

function main()
end
""")
        ))

        assertEquals(2, state.modules.size)
        val module = state.modules["my.mod.name"]
        assertNotNull(module)

        val func = module!!.functions()["main"]!!.first()
        assertEquals("main", func.name)
        assertEquals(0, func.parameters.size)
        assertEquals(VoidType, func.returnType)
    }

    @Test
    fun test_IllegalModuleName() {
        try {
            val state = compile(listOf(
                    Source("test.kek", """
module my.module.name
""")
            ))
            assertTrue("Should have thrown exception", false)
        } catch(e: CompilerException) {
            assertEquals(2, e.line)
            assertEquals(11, e.columnStart)
        }
    }

    @Test
    fun test_MergeModules() {
        val state = compile(listOf(
                Source("test.kek", """
module m

function a()
end
"""),
                Source("test2.kek", """
module m

function b()
end
""")
        ))

        assertEquals(2, state.modules.size)
        val module = state.modules["m"]
        assertNotNull(module)
        assertTrue(module!!.functions().containsKey("a"))
        assertTrue(module!!.functions().containsKey("b"))
    }

    @Test
    fun test_SimpleImport() {
        val state = compile(listOf(
                Source("math.kek", """
module math

function a()
end
"""),
                Source("test.kek", """
import math

function b()
    a()
end
""")
        ))

        assertEquals(2, state.modules.size)
        val module = state.modules[""]
        assertNotNull(module)
    }

    @Test
    fun test_MissingImport() {
        try {
            val state = compile(listOf(
                    Source("test.kek", """
import math
""")
            ))
            assertTrue("Should have thrown exception", false)
        } catch (e: CompilerException) {
            assertEquals(2, e.line)
        }
    }
}