package Triangle.SyntacticAnalyzer

import spock.lang.Specification
import spock.lang.Title

@Title('Syntactic analyzer')
class ScannerSpec extends Specification {

    private static final String SIMPLE_TRIANGLE_FILE = 'src/test/resources/simple.tm'

    def 'How tokens are created'() {

        given: 'Triangle source file reader and scanner'
        SourceFile sf = new SourceFile(SIMPLE_TRIANGLE_FILE)
        Scanner scanner = new Scanner(sf)
        scanner.enableDebugging()

        when: 'Start reading file, like compiler do'

        then: 'First token should be displayed'
        Token letToken = new Token(13, 'let', new SourcePosition(1, 1))
        assert scanner.scan() == letToken
    }
}