package EasyDoesIt.Easy.SyntacticAnalizer;

import EasyDoesIt.Easy.AbstractSyntaxTrees.*;
import EasyDoesIt.Easy.ErrorReporter;

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

    void accept(int tokenExpected) throws SyntaxError {

        if (currentToken.kind == tokenExpected) {
            previousTokenPosition = currentToken.position;
            currentToken = lexicalAnalyser.scan();

        } else {
            syntacticError("\"%\" expected here", Token.spell(tokenExpected));
        }
    }

    void acceptIt() {
        previousTokenPosition = currentToken.position;
        currentToken = lexicalAnalyser.scan();
    }

    void start(SourcePosition position) {
        position.start = currentToken.position.start;
    }

    void finish(SourcePosition position) {
        position.finish = previousTokenPosition.finish;
    }

    void syntacticError(String messageTemplate, String tokenQuoted) throws SyntaxError {
        SourcePosition pos = currentToken.position;
        errorReporter.reportError(messageTemplate, tokenQuoted, pos);

        throw new SyntaxError();
    }

//_____________________________________________________________________________
//                                                                     PROGRAM

    public Program parseProgram() {

        Program programAST;

        // initialize, because everything starts from here
        previousTokenPosition.start = 0;
        previousTokenPosition.finish = 0;

        currentToken = lexicalAnalyser.scan();

        try {

            Segment sAST = parseSegment();
            programAST = new Program(previousTokenPosition, sAST);

            if (currentToken.kind != Token.EOT) {
                syntacticError("\"%\" not expected after end of program", currentToken.spelling);
            }

        } catch (SyntaxError s) {
            return null;
        }

        return programAST;
    }

//_____________________________________________________________________________
//                                                                     Program

    private Segment parseSegment() throws SyntaxError {
        return null;
    }

//_____________________________________________________________________________
//                                                                 Expressions

    Expression parseExpression() throws SyntaxError {
        return null;
    }

//_____________________________________________________________________________
//                                                                    Literals

    Identifier parseIdentifier() throws SyntaxError {
        Identifier I;

        if (currentToken.kind == Token.IDENTIFIER) {

            previousTokenPosition = currentToken.position;
            String spelling = currentToken.spelling;

            I = new Identifier(previousTokenPosition, spelling);

            currentToken = lexicalAnalyser.scan();

        } else {
            I = null;
            syntacticError("identifier expected here", "");
        }

        return I;
    }

//_____________________________________________________________________________
//                                                                   Variables

    Vname parseVariable() throws SyntaxError {

        Vname vnameAST;

        Identifier iAST = parseIdentifier();
        vnameAST = parseRestOfVname(iAST);

        return vnameAST;
    }

    Vname parseRestOfVname(Identifier identifierAST) throws SyntaxError {

        SourcePosition srcPos = identifierAST.position;

        Vname vAST = new SimpleVname(srcPos, identifierAST);

        while (currentToken.kind == Token.DOT || currentToken.kind == Token.LBRACKET) {

            if (currentToken.kind == Token.DOT) {
                acceptIt();

                Identifier iAST = parseIdentifier();
                vAST = new DotVname(srcPos, vAST, iAST);

            } else {
                acceptIt();

                Expression eAST = parseExpression();
                accept(Token.RBRACKET);
                finish(srcPos);

                vAST = new SubscriptVname(srcPos, vAST, eAST);
            }
        }

        return vAST;
    }
}
