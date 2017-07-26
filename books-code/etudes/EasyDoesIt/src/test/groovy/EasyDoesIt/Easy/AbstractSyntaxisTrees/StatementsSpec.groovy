package EasyDoesIt.Easy.AbstractSyntaxisTrees

import EasyDoesIt.Easy.AbstractSyntaxTrees.AST
import EasyDoesIt.Easy.SyntacticAnalizer.Parser

class StatementsSpec extends ASTSpec {

    def 'Assignments definition'() {

        given: 'Parser and assignment'
        Parser parser = getParserFor('SET variable1 := 0;')

        when: 'Parser finish'

        then: 'AST should be constructed'
        AST theAST = parser.parseProgram();
        assert theAST
    }

    def 'Multiple assignments definition'() {

        given: 'Parser and assignment'
        Parser parser = getParserFor('SET variable1, variable2 := 0;')

        when: 'Parser finish'

        then: 'AST should be constructed'
        AST theAST = parser.parseProgram();
        assert theAST
    }

    def 'Call definition without parameters'() {

        given: 'Parser and assignment'
        Parser parser = getParserFor('CALL procedureName;')

        when: 'Parser finish'

        then: 'AST should be constructed'
        AST theAST = parser.parseProgram();
        assert theAST
    }

    def 'Call definition with three parameters'() {

        given: 'Parser and assignment'
        Parser parser = getParserFor('CALL procedureName(1, 2, 3, 4);')

        when: 'Parser finish'

        then: 'AST should be constructed'
        AST theAST = parser.parseProgram();
        assert theAST
    }
}