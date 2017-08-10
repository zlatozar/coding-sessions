package EasyDoesIt.Easy.ContextualAnalizer

import EasyDoesIt.Easy.ASTSpec
import EasyDoesIt.Easy.AbstractSyntaxTrees.Program
import EasyDoesIt.Easy.SyntacticAnalizer.Parser

class TypeEqualitySpec extends ASTSpec {

    private final String STRUCTURE_TYPE =
            'TYPE fistName1 IS\n' +
                    'STRUCTURE\n'+
                    'FIELD firstField1 IS STRING,\n' +
                    'FIELD secondField1 IS INTEGER,\n' +
                    'FIELD thirdField1 IS REAL\n' +
                    'END STRUCTURE;' +

            'TYPE fistName2 IS\n' +
                    'STRUCTURE\n'+
                    'FIELD firstField2 IS STRING,\n' +
                    'FIELD secondField2 IS INTEGER,\n' +
                    'FIELD thirdField2 IS REAL\n' +
                    'END STRUCTURE;'

    def 'How type equality works'() {

        given: 'Easy language source file reader, scanner, parser and checker'
        Parser parser = getParserFor(STRUCTURE_TYPE)

        when: 'Second pass starts'
        Program programAST = parser.parseProgram()

        then: 'AST structure will be traced'
        getChecker().check(programAST)
    }
}