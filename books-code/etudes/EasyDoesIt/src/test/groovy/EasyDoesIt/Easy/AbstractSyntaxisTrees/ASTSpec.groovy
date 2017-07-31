package EasyDoesIt.Easy.AbstractSyntaxisTrees

import EasyDoesIt.Easy.ErrorReporter
import EasyDoesIt.Easy.SyntacticAnalizer.Parser
import EasyDoesIt.Easy.SyntacticAnalizer.Scanner
import EasyDoesIt.Easy.SyntacticAnalizer.SourceFile
import spock.lang.Specification

class ASTSpec extends Specification {

    protected static Parser getParserFor(String statement) {
        String program = createProgram(statement)

        SourceFile sourceFile = new SourceFile(program, false)
        Scanner scanner = new Scanner(sourceFile);
        ErrorReporter reporter = new ErrorReporter();

        return new Parser(scanner, reporter);
    }

    protected static String createProgram(String statement) {
        return "PROGRAM simpleProgram:\n" +
                "$statement\n" +
                ";" +
                "END PROGRAM simple;"
    }

    def 'dummy'() {
        given: 'some test'
        when: 'call it'
        then: assert true
    }
}