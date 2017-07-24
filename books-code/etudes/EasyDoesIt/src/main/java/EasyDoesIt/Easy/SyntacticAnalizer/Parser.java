package EasyDoesIt.Easy.SyntacticAnalizer;

import EasyDoesIt.Easy.AbstractSyntaxTrees.*;
import EasyDoesIt.Easy.ErrorReporter;

/**
 * In parsing it is convenient to view the source program as a stream of tokens: symbols
 * such as identifiers, literals, operators, keywords, and punctuation.
 *
 * The parser treats each token as a terminal symbol. Note that the parser examines only
 * the kind of the current token, ignoring its spelling.
 */
public class Parser {

    private final Scanner lexicalAnalyser;
    private final ErrorReporter errorReporter;

    private Token currentToken;
    private SourcePosition previousTokenPosition;

    public Parser(Scanner lexer, ErrorReporter reporter) {
        this.lexicalAnalyser = lexer;
        this.errorReporter = reporter;
        this.previousTokenPosition = new SourcePosition();
    }

    /**
     * <p/>
     * If so, fetches the next token.
     * If not, reports a syntactic error.
     */
    void accept(int tokenExpected) throws SyntaxError {

        if (currentToken.kind == tokenExpected) {
            previousTokenPosition = currentToken.position;
            currentToken = lexicalAnalyser.scan();

        } else {
            syntacticError("\"%\" expected here", Token.spell(tokenExpected));

        }
    }

    /**
     * Unconditionally fetches the next token from the source
     */
    void acceptIt() {
        previousTokenPosition = currentToken.position;
        currentToken = lexicalAnalyser.scan();
    }

    /**
     * Records the position of the start of a phrase.
     * This is defined to be the position of the first
     * character of the first token of the phrase.
     */
    void start(SourcePosition position) {
        position.start = currentToken.position.start;
    }

    /**
     * Records the position of the end of a phrase.
     * This is defined to be the position of the last
     * character of the last token of the phrase.
     */
    void finish(SourcePosition position) {
        position.finish = previousTokenPosition.finish;
    }

    void syntacticError(String messageTemplate, String tokenQuoted) throws SyntaxError {
        SourcePosition pos = currentToken.position;
        errorReporter.reportError(messageTemplate, tokenQuoted, pos);

        throw new SyntaxError();
    }

// Recursive descent is a top-down parsing algorithm. A recursive-descent parser for a
// grammar G consists of a group of methods 'parseN', one for each nonterminal symbol
// N of G. The task of each method 'parseN' is to parse a single N-phrase. These parsing
// methods cooperate to parse complete sentences.

//_____________________________________________________________________________
//                                                                    PROGRAMS

//    <program>      ::=  <program head> <segment body> <program end>
//    <program head> ::=  PROGRAM <identifier> :
//    <program end>  ::=  END PROGRAM <indentifier> ;

    public Program parseProgram() {

        Program programAST;

        // initialize, because everything starts from here
        previousTokenPosition.start = 0;
        previousTokenPosition.finish = 0;

        currentToken = lexicalAnalyser.scan();

        try {
            ProgramHead phAST = parseProgramHead();
            SegmentBody sbAST = parseSegmentBody();
            ProgramEnd peAST = parseProgramEnd();

            programAST = new Program(phAST, sbAST, peAST, previousTokenPosition);

            if (currentToken.kind != Token.EOT) {
                syntacticError("\"%\" not expected after end of program", currentToken.spelling);
            }

        } catch (SyntaxError s) {
            return null;
        }

        return programAST;
    }

    private ProgramHead parseProgramHead() throws SyntaxError {

        // in case there's a syntactic error
        ProgramHead phAST = null;

        SourcePosition programHeadPos = new SourcePosition();
        start(programHeadPos);

        if (currentToken.kind == Token.PROGRAM) {
            acceptIt();

            Identifier iAST = parseIdentifier();
            phAST = new ProgramHead(iAST, programHeadPos);
            accept(Token.COLON);

            finish(programHeadPos);

        } else {
            syntacticError("\"%\" cannot start a program", currentToken.spelling);
        }

        return phAST;
    }


    private SegmentBody parseSegmentBody() throws SyntaxError {

        // in case there's a syntactic error
        SegmentBody sbAST = null;

        SourcePosition sbPos = new SourcePosition();
        start(sbPos);

        switch (currentToken.kind) {

            case Token.SEMICOLON:
                acceptIt();
                NullStatement nullStatement = parseNullStatement();

                sbAST = new SegmentBody(nullStatement, sbPos);
                finish(sbPos);

                break;

            default:
                syntacticError("\"%\" cannot start segment", currentToken.spelling);

        }

        return sbAST;
    }

    private ProgramEnd parseProgramEnd() throws SyntaxError {

        // in case there's a syntactic error
        ProgramEnd peAST = null;

        SourcePosition pePos = new SourcePosition();
        start(pePos);

        if (currentToken.kind == Token.END) {
            acceptIt();
            accept(Token.PROGRAM);

            Identifier iAST = parseIdentifier();
            peAST = new ProgramEnd(iAST, pePos);
            accept(Token.SEMICOLON);

            finish(pePos);

        } else {
            syntacticError("\"%\" is expected here", currentToken.spelling);
        }

        return peAST;
    }

//_____________________________________________________________________________
//                                                                    LITERALS


    /**
     * Parses an identifier, and constructs a leaf AST to
     * represent it.
     *
     * Note: The nonterminal symbol identifier corresponds to a single token,
     * so the method is similar to {@link Parser#accept}
     */
    Identifier parseIdentifier() throws SyntaxError {
        Identifier I;

        if (currentToken.kind == Token.IDENTIFIER) {
            previousTokenPosition = currentToken.position;
            String spelling = currentToken.spelling;

            I = new Identifier(spelling, previousTokenPosition);

            currentToken = lexicalAnalyser.scan();

        } else {
            I = null;
            syntacticError("identifier expected here", "");
        }

        return I;
    }


//_____________________________________________________________________________
//                                                                  STATEMENTS

    private NullStatement parseNullStatement() throws SyntaxError {

        SourcePosition nsPos = new SourcePosition();
        start(nsPos);

        NullStatement nsAST = new NullStatement(currentToken.spelling, nsPos);
        finish(nsPos);

        return nsAST;
    }

//_____________________________________________________________________________
//                                                                 EXPRESSIONS

//_____________________________________________________________________________
//                                                     VALUE-OR-VARIABLE NAMES


//_____________________________________________________________________________
//                                                                DECLARATIONS


//_____________________________________________________________________________
//                                                                  PARAMETERS


//_____________________________________________________________________________
//                                                               TYPE-DENOTERS

}
