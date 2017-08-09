package EasyDoesIt.Easy.ContextualAnalizer

import EasyDoesIt.Easy.AbstractSyntaxTrees.Program
import EasyDoesIt.Easy.ErrorReporter
import EasyDoesIt.Easy.SyntacticAnalizer.Parser
import EasyDoesIt.Easy.SyntacticAnalizer.Scanner
import EasyDoesIt.Easy.SyntacticAnalizer.SourceFile

import spock.lang.Specification

class CheckerSpec extends Specification {

    private static final String SIMPLE_TRIANGLE_FILE = 'src/test/resources/simple.tri'

    def 'How checker works'() {

        given: 'Triangle source file reader, scanner, parser and checker'
        SourceFile sf = new SourceFile(SIMPLE_TRIANGLE_FILE)
        Scanner scanner = new Scanner(sf)
        ErrorReporter errorReporter = new ErrorReporter()

        Parser parser = new Parser(scanner, errorReporter)
        Checker checker = new Checker(errorReporter)

        when: 'Start reading file, like compiler do'

        then: 'First token should be displayed'
        Program programAST = parser.parseProgram()

        checker.check(programAST)

        assert programAST
    }

}