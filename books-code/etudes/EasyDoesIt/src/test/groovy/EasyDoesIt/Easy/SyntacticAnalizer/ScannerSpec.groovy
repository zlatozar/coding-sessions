package EasyDoesIt.Easy.SyntacticAnalizer

import spock.lang.Specification
import spock.lang.Title

@Title('Scanner specification')
class ScannerSpec extends Specification {

    private String sample = '/*Sample*/'

    private String empty = ' /**/ '

    private String manyLines = '/*\n' +
            'First line\n' +
            'Second line\n' +
            '*/'
    private String weird = '/* Weird one\n' +
            '\n' +
            '\n' +
            '\n' +
            '\n' +
            '*/'

    Token currentToken

    def 'Sample C-style comment'() {

        given: 'Scanner and source that contains only one comment'
        SourceFile sourceFile = new SourceFile(sample, false)
        Scanner scanner = new Scanner(sourceFile)

        when: 'Scanning starts'
        scanner.enableDebugging()
        currentToken = scanner.scan()

        then: 'Sample one'
        assert currentToken.kind == 3
    }

    def 'Empty C-style comment'() {

        given: 'Scanner and source that contains only one comment'
        SourceFile sourceFile = new SourceFile(empty, false)
        Scanner scanner = new Scanner(sourceFile)

        when: 'Scanning starts'
        scanner.enableDebugging()
        currentToken = scanner.scan()

        then: 'Empty one'
        assert currentToken.kind == 3
    }

    def 'Many lines C-style comment'() {

        given: 'Scanner and source that contains only one comment but with many lines'
        SourceFile sourceFile = new SourceFile(manyLines, false)
        Scanner scanner = new Scanner(sourceFile)

        when: 'Scanning starts'
        scanner.enableDebugging()
        currentToken = scanner.scan()

        then: 'Many lines'
        assert currentToken.kind == 3
    }

    def 'Ugly C-style comment'() {

        given: 'Scanner and source that contains only one comment but with many lines'
        SourceFile sourceFile = new SourceFile(weird, false)
        Scanner scanner = new Scanner(sourceFile)

        when: 'Scanning starts'
        scanner.enableDebugging()
        currentToken = scanner.scan()

        then: 'Many weird lines'
        assert currentToken.kind == 3
    }
}