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
    private final String TYPE_IDENTIFIER_SEQ = createProgram('TYPE firstName IS INTEGER;\nTYPE secondName IS BOOLEAN;')

    private final String ARRAYED_TYPE = createProgram('TYPE firstName IS ARRAY [5] OF INTEGER;')
    private final String ARRAYED_TYPE_SECTION = createProgram('TYPE firstName IS ARRAY [5:15] OF STRING;')

    private final String ARRAYED_TYPE_SEQ = createProgram('TYPE firstName IS ARRAY [5:15] OF STRING;\nTYPE secondName IS ARRAY [5] OF INTEGER;')

    private final String STRUCTURE_TYPE = createProgram(
            'TYPE fistName IS\n' +
                    'STRUCTURE\n'+
                    'FIELD firstField IS STRING\n' +
                    'END STRUCTURE;'
    )

    private final String STRUCTURE_TYPE_MANY_FIELDS = createProgram(
            'TYPE fistName IS\n' +
                    'STRUCTURE\n'+
                    'FIELD firstField IS STRING,\n' +
                    'FIELD secondField IS INTEGER\n' +
                    'END STRUCTURE;'
    )

    def 'Type definition'() {

        given: 'Parser a simple type identifier'
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

    def 'Parse a simple arrayed type definition'() {

        given: 'Parser and arrayed type'
        SourceFile sourceFile = new SourceFile(ARRAYED_TYPE, false)
        Scanner scanner = new Scanner(sourceFile);
        ErrorReporter reporter = new ErrorReporter();
        Parser parser = new Parser(scanner, reporter);

        when: 'Parser finish'

        then: 'AST should be constructed'
        AST theAST = parser.parseProgram();
        assert theAST
    }

    def 'Parse a arrayed type with section definition'() {

        given: 'Parser and arrayed type'
        SourceFile sourceFile = new SourceFile(ARRAYED_TYPE_SECTION, false)
        Scanner scanner = new Scanner(sourceFile);
        ErrorReporter reporter = new ErrorReporter();
        Parser parser = new Parser(scanner, reporter);

        when: 'Parser finish'

        then: 'AST should be constructed'
        AST theAST = parser.parseProgram();
        assert theAST
    }

    def 'Parse a arrayed type sequence definition'() {

        given: 'Parser and arrayed type sequence'
        SourceFile sourceFile = new SourceFile(ARRAYED_TYPE_SEQ, false)
        Scanner scanner = new Scanner(sourceFile);
        ErrorReporter reporter = new ErrorReporter();
        Parser parser = new Parser(scanner, reporter);

        when: 'Parser finish'

        then: 'AST should be constructed'
        AST theAST = parser.parseProgram();
        assert theAST
    }

    def 'Parse a structured type sequence definition'() {

        given: 'Parser and structured type sequence'
        SourceFile sourceFile = new SourceFile(STRUCTURE_TYPE, false)
        Scanner scanner = new Scanner(sourceFile);
        ErrorReporter reporter = new ErrorReporter();
        Parser parser = new Parser(scanner, reporter);

        when: 'Parser finish'

        then: 'AST should be constructed'
        AST theAST = parser.parseProgram();
        assert theAST
    }

    def 'Parse a structured type sequence definition with many fields'() {

        given: 'Parser and structured type sequence'
        SourceFile sourceFile = new SourceFile(STRUCTURE_TYPE_MANY_FIELDS, false)
        Scanner scanner = new Scanner(sourceFile);
        ErrorReporter reporter = new ErrorReporter();
        Parser parser = new Parser(scanner, reporter);

        when: 'Parser finish'

        then: 'AST should be constructed'
        AST theAST = parser.parseProgram();
        assert theAST
    }
}