package Triangle.SyntacticAnalyzer

import Triangle.ErrorReporter
import spock.lang.Specification
import spock.lang.Title

@Title('Syntactic analyzer')
class ParserSpec extends Specification {

    private static final String SIMPLE_TRIANGLE_FILE = 'src/test/resources/simple.tm'

    def 'How parser works'() {

        given: 'Triangle source file reader, scanner and parser'
        SourceFile sf = new SourceFile(SIMPLE_TRIANGLE_FILE)
        Scanner scanner = new Scanner(sf)
        scanner.enableDebugging()

        Parser parser = new Parser(scanner, new ErrorReporter())

        when: 'Start reading file, like compiler do'

        then: 'First token should be displayed'
        parser.parseProgram()
    }
}