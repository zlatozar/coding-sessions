package EasyDoesIt.Easy.AbstractSyntaxisTrees

import EasyDoesIt.Easy.AbstractSyntaxTrees.AST
import EasyDoesIt.Easy.ErrorReporter
import EasyDoesIt.Easy.SyntacticAnalizer.Parser
import EasyDoesIt.Easy.SyntacticAnalizer.Scanner
import EasyDoesIt.Easy.SyntacticAnalizer.SourceFile
import spock.lang.Specification
import spock.lang.Title

@Title('AST')
class ProgramSpec extends Specification {

    private static final String PROGRAM_STRUCTURE =
            'PROGRAM simple:\n' +
            ';\n' +
            'END PROGRAM simple;'


    def 'Program structure'() {

        given: 'Parser and simple program'
        SourceFile sourceFile = new SourceFile(PROGRAM_STRUCTURE, false)
        Scanner scanner = new Scanner(sourceFile);
        ErrorReporter reporter = new ErrorReporter();
        Parser parser = new Parser(scanner, reporter);

        when: 'Parser pass'

        then: 'AST should be constructed'
        AST theAST = parser.parseProgram();
        assert theAST
    }

}