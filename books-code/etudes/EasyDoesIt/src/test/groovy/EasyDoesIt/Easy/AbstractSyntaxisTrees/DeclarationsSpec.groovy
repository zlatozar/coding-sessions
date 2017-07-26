package EasyDoesIt.Easy.AbstractSyntaxisTrees

import EasyDoesIt.Easy.AbstractSyntaxTrees.AST
import EasyDoesIt.Easy.ErrorReporter
import EasyDoesIt.Easy.SyntacticAnalizer.Parser
import EasyDoesIt.Easy.SyntacticAnalizer.Scanner
import EasyDoesIt.Easy.SyntacticAnalizer.SourceFile
import spock.lang.Title

@Title('AST')
class DeclarationsSpec extends ASTSpec {

    private final String DECLARATION_IDENTIFIER = 'DECLARE epsilon REAL;'
    private final String GROUP_DECLARATION_IDENTIFIER = 'DECLARE (epsilon, pi) REAL;'

    def 'Declaration definition'() {

        given: 'Parser a simple type identifier'
        Parser parser = getParserFor(DECLARATION_IDENTIFIER)

        when: 'Parser finish'

        then: 'AST should be constructed'
        AST theAST = parser.parseProgram();
        assert theAST
    }

    def 'Group declaration definition'() {

        given: 'Parser a simple type identifier'
        Parser parser = getParserFor(GROUP_DECLARATION_IDENTIFIER)

        when: 'Parser finish'

        then: 'AST should be constructed'
        AST theAST = parser.parseProgram();
        assert theAST
    }

}