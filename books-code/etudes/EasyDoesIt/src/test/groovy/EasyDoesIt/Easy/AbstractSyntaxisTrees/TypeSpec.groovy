package EasyDoesIt.Easy.AbstractSyntaxisTrees

import EasyDoesIt.Easy.AbstractSyntaxTrees.AST
import EasyDoesIt.Easy.ErrorReporter
import EasyDoesIt.Easy.SyntacticAnalizer.Parser
import EasyDoesIt.Easy.SyntacticAnalizer.Scanner
import EasyDoesIt.Easy.SyntacticAnalizer.SourceFile
import spock.lang.Title

@Title('AST')
class TypeSpec extends ASTSpec {

    private final String TYPE_IDENTIFIER = createProgram('TYPE simple IS INTEGER;')
    private final String TYPE_IDENTIFIER_SEQ = createProgram('TYPE simple IS INTEGER;\nTYPE newOne IS BOOLEAN;')

    def 'Type definition'() {

        given: 'Parser a simple program'
        SourceFile sourceFile = new SourceFile(TYPE_IDENTIFIER, false)
        Scanner scanner = new Scanner(sourceFile);
        ErrorReporter reporter = new ErrorReporter();
        Parser parser = new Parser(scanner, reporter);

        when: 'Parser finish'

        then: 'AST should be constructed'
        AST theAST = parser.parseProgram();
        assert theAST
    }

    def 'Sequence of type definitions'() {

        given: 'Parser and simple program'
        SourceFile sourceFile = new SourceFile(TYPE_IDENTIFIER_SEQ, false)
        Scanner scanner = new Scanner(sourceFile);
        ErrorReporter reporter = new ErrorReporter();
        Parser parser = new Parser(scanner, reporter);

        when: 'Parser finish'

        then: 'AST should be constructed'
        AST theAST = parser.parseProgram();
        assert theAST
    }

}