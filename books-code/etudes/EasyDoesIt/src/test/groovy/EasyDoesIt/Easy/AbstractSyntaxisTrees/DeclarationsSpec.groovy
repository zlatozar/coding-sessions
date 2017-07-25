package EasyDoesIt.Easy.AbstractSyntaxisTrees

import EasyDoesIt.Easy.AbstractSyntaxTrees.AST
import EasyDoesIt.Easy.ErrorReporter
import EasyDoesIt.Easy.SyntacticAnalizer.Parser
import EasyDoesIt.Easy.SyntacticAnalizer.Scanner
import EasyDoesIt.Easy.SyntacticAnalizer.SourceFile
import spock.lang.Title

@Title('AST')
class DeclarationsSpec extends ASTSpec {

    private final String DECLARATION_IDENTIFIER = createProgram('DECLARE epsilon REAL;')
    private final String GROUP_DECLARATION_IDENTIFIER = createProgram('DECLARE (epsilon, pi) REAL;')

    def 'Declaration definition'() {

        given: 'Parser a simple type identifier'
        SourceFile sourceFile = new SourceFile(DECLARATION_IDENTIFIER, false)
        Scanner scanner = new Scanner(sourceFile);
        ErrorReporter reporter = new ErrorReporter();
        Parser parser = new Parser(scanner, reporter);

        when: 'Parser finish'

        then: 'AST should be constructed'
        AST theAST = parser.parseProgram();
        assert theAST
    }

    def 'Group declaration definition'() {

        given: 'Parser a simple type identifier'
        SourceFile sourceFile = new SourceFile(GROUP_DECLARATION_IDENTIFIER, false)
        Scanner scanner = new Scanner(sourceFile);
        ErrorReporter reporter = new ErrorReporter();
        Parser parser = new Parser(scanner, reporter);

        when: 'Parser finish'

        then: 'AST should be constructed'
        AST theAST = parser.parseProgram();
        assert theAST
    }

}