package EasyDoesIt.Easy.SyntacticAnalizer;

import EasyDoesIt.Easy.AbstractSyntaxTrees.*;
import EasyDoesIt.Easy.ErrorReporter;

/**
 * In parsing it is convenient to view the source program as a stream of tokens: symbols
 * such as identifiers, literals, operators, keywords, and punctuation.
 * <p/>
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

            case Token.TYPE:
                TypeDefinition typeDefinition = parseTypeDefinition();

                sbAST = new TypeDefinitionSegmentBody(typeDefinition, sbPos);

                while (currentToken.kind == Token.TYPE) {
                    TypeDefinition typeDefinition2 = parseTypeDefinition();

                    finish(sbPos);

                    sbAST = new TypeDefinitionSequenceSegmentBody(typeDefinition, typeDefinition2, sbPos);
                }
                break;

            case Token.DECLARE:
                VariableDeclaration variableDeclaration = parseVariableDeclaration();

                sbAST = new VariableDeclarationSegmentBody(variableDeclaration, sbPos);

                while (currentToken.kind == Token.COMMA) {
                    VariableDeclaration variableDeclaration2 = parseVariableDeclaration();

                    finish(sbPos);

                    sbAST = new VariableDeclarationSequenceSegmentBody(variableDeclaration, variableDeclaration2, sbPos);
                }
                break;

            // TODO
            case Token.SEMICOLON:
                NullStatement nullStatement = parseNullStatement();

                finish(sbPos);

                sbAST = new NullSegmentBody(nullStatement, sbPos);
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
     * <p/>
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

        acceptIt();
        finish(nsPos);

        return new NullStatement(currentToken.spelling, nsPos);
    }

//_____________________________________________________________________________
//                                                                 EXPRESSIONS

//_____________________________________________________________________________
//                                                     VALUE-OR-VARIABLE NAMES

//_____________________________________________________________________________
//                                                                  PARAMETERS


//_____________________________________________________________________________
//                                                               TYPE-DENOTERS

    public TypeDefinition parseTypeDefinition() throws SyntaxError {

        TypeDefinition typeDefinition = null;

        SourcePosition tdPos = new SourcePosition();
        start(tdPos);

        if (currentToken.kind == Token.TYPE) {
            acceptIt();
            Identifier identifier = parseIdentifier();

            accept(Token.IS);

            Type type = parseType();
            accept(Token.SEMICOLON);

            finish(tdPos);

            typeDefinition = new TypeDefinition(identifier, type, tdPos);

        } else {
            syntacticError("\"%\" cannot start a declaration", currentToken.spelling);
        }

        return typeDefinition;
    }

    public Type parseType() throws SyntaxError {

        Type type = null;

        SourcePosition tPos = new SourcePosition();
        start(tPos);

        switch (currentToken.kind) {

            case Token.IDENTIFIER:
                Identifier identifier = parseIdentifier();
                finish(tPos);

                type = new TypeIdentifier(identifier, tPos);
                break;

            case Token.ARRAY:
                acceptIt();

                type = parseArrayedTypeDefinition();
                finish(tPos);
                break;

            case Token.STRUCTURE:
                acceptIt();

                type = parseStructureType();
                finish(tPos);
                break;

            default:
                syntacticError("\"%\" cannot start a type", currentToken.spelling);

        }

        return type;
    }

    public ArrayedTypeDefinition parseArrayedTypeDefinition() throws SyntaxError {

        SourcePosition atPos = new SourcePosition();
        start(atPos);

        Bounds bounds = parseBounds();
        accept(Token.OF);

        Type type = parseType();
        finish(atPos);

        return new ArrayedTypeDefinition(bounds, type, atPos);
    }

    public Bounds parseBounds() throws SyntaxError {

        Bounds bounds;

        SourcePosition atPos = new SourcePosition();
        start(atPos);

        if (currentToken.kind == Token.LBRACKET) {
            acceptIt();

        } else {
            syntacticError("\"%\" cannot start an array bounds", currentToken.spelling);
        }

        Expression expression = parseExpression();

        if (currentToken.kind == Token.COLON) {
            acceptIt();

            Expression expression2 = parseExpression();
            finish(atPos);

            bounds = new BoundSection(expression, expression2, atPos);
            accept(Token.RBRACKET);

        } else {
            accept(Token.RBRACKET);
            bounds = new BoundPosition(expression, atPos);
        }

        return bounds;
    }

    public StructureType parseStructureType() throws SyntaxError {

        SourcePosition stPos = new SourcePosition();
        start(stPos);

        FieldList fieldList = parseFieldList();

        accept(Token.END);
        accept(Token.STRUCTURE);

        finish(stPos);

        return new StructureType(fieldList, stPos);
    }

    public FieldList parseFieldList() throws SyntaxError {

        FieldList fieldList;

        SourcePosition flPos = new SourcePosition();
        start(flPos);

        Field field = parseField();
        fieldList = new FieldListSingle(field, flPos);

        while (currentToken.kind == Token.COMMA) {
            acceptIt();

            Field field2 = parseField();
            finish(flPos);

            fieldList = new FieldListSequence(field, field2, flPos);
        }

        return fieldList;
    }

    public Field parseField() throws SyntaxError {

        Field field = null;

        SourcePosition fPos = new SourcePosition();
        start(fPos);

        if (currentToken.kind == Token.FIELD) {
            acceptIt();

            Identifier identifier = parseIdentifier();
            accept(Token.IS);

            Type type = parseType();
            finish(fPos);

            field = new Field(identifier, type, fPos);

        } else {
            syntacticError("\"%\" cannot start a field", currentToken.spelling);
        }

        return field;
    }

    public Expression parseExpression() throws SyntaxError {
        acceptIt();
        return null;
    }

//_____________________________________________________________________________
//                                                                DECLARATIONS

    public VariableDeclaration parseVariableDeclaration() throws SyntaxError {

        VariableDeclaration variableDeclaration = null;

        SourcePosition vnPos = new SourcePosition();
        start(vnPos);

        if (currentToken.kind == Token.DECLARE) {
            acceptIt();

            Declarations declaredNames = parseDeclaredNames();
            Type type = parseType();

            finish(vnPos);

            variableDeclaration = new VariableDeclaration(declaredNames, type, vnPos);

        } else {
            syntacticError("\"%\" cannot start a declaration", currentToken.spelling);
        }

        accept(Token.SEMICOLON);

        return variableDeclaration;
    }

    public Declarations parseDeclaredNames() throws SyntaxError {

        Declarations declaredNames = null;

        SourcePosition dnPos = new SourcePosition();
        start(dnPos);

        if (currentToken.kind == Token.IDENTIFIER) {
            Identifier identifier = parseIdentifier();
            finish(dnPos);

            declaredNames = new DeclaredNames(identifier, dnPos);

        } else if (currentToken.kind == Token.LPAREN) {
            acceptIt();

            Identifier identifier = parseIdentifier();
            finish(dnPos);

            while (currentToken.kind == Token.COMMA) {
                acceptIt();

                Identifier identifier2 = parseIdentifier();
                finish(dnPos);

                declaredNames = new DeclaredNamesSequence(identifier, identifier2, dnPos);
            }

            accept(Token.RPAREN);

        } else {
            syntacticError("\"%\" cannot start a names declaration", currentToken.spelling);

        }

        return declaredNames;
    }

}
