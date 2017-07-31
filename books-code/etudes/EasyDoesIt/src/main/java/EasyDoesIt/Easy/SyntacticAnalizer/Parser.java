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
//                                                                 Entry point

    public Program parseProgram() {

        Program programAST;

        // initialize, because everything starts from here
        previousTokenPosition.start = 0;
        previousTokenPosition.finish = 0;

        currentToken = lexicalAnalyser.scan();

        try {

            ProgramBody prgAST = parseProgramBody();
            programAST = new Program(previousTokenPosition, prgAST);

            if (currentToken.kind != Token.EOT) {
                syntacticError("\"%\" not expected after end of program", currentToken.spelling);
            }

        } catch (SyntaxError s) {
            return null;
        }

        return programAST;
    }

//_____________________________________________________________________________
//         PROGRAM contains definitions and executable statements
//_____________________________________________________________________________

    private ProgramBody parseProgramBody() throws SyntaxError {

        SourcePosition srcPos = new SourcePosition();
        start(srcPos);

        accept(Token.PROGRAM);
        Identifier prgName = parseIdentifier();
        accept(Token.COLON);

        Definition prgDefinition = parseDefinitionsSeq();
        Statement prgStatement = parseExecutableStatementSeq();

        Command command = new Command(srcPos, prgDefinition, prgStatement);

        accept(Token.END);
        accept(Token.PROGRAM);

        Identifier prgNameEnd = parseIdentifier();
        accept(Token.SEMICOLON);

        return new ProgramBody(srcPos, prgName, command, prgNameEnd);
    }

//_____________________________________________________________________________
//                   DEFINITIONS has segments interface
//_____________________________________________________________________________


    Definition parseDefinitionsSeq() throws SyntaxError {

        Definition definition;

        SourcePosition srcPos = new SourcePosition();
        start(srcPos);

        if (isSegment()) {
            definition = parseSingleDefinition();

        } else {
            definition = new EmptyDefinition(srcPos);
        }

        return definition;
    }

    Definition parseSingleDefinition() throws SyntaxError {

        SourcePosition srcPos = new SourcePosition();
        start(srcPos);

        Definition definition = parseDefinition();
        finish(srcPos);

        while (isSegment()) {
            Definition definition2 = parseDefinition();
            finish(srcPos);

            definition = new DefinitionSeq(srcPos, definition, definition2);
        }

        return definition;
    }

    Definition parseDefinition() throws SyntaxError {

        Definition definition = null;

        SourcePosition srcPos = new SourcePosition();
        start(srcPos);

        switch (currentToken.kind) {

            case Token.TYPE:
                definition = parseTypeDefinition();

                finish(srcPos);
                break;

            case Token.DECLARE:
                definition = parseDeclaration();

                finish(srcPos);
                break;

            case Token.FUNCTION:
            case Token.PROCEDURE:
                definition = parseInternalProcedure();

                finish(srcPos);
                break;

            default:
                syntacticError("\"%\" cannot start a segment", currentToken.spelling);
        }

        return definition;
    }

    private boolean isSegment() {
        int token = currentToken.kind;
        return token == Token.TYPE || token == Token.DECLARE || token == Token.FUNCTION || token == Token.PROCEDURE;
    }

//_____________________________________________________________________________
//                                                             Type definition

    Definition parseTypeDefinition() throws SyntaxError {
        SourcePosition srcPos = new SourcePosition();
        start(srcPos);

        accept(Token.TYPE);

        Identifier typeName = parseIdentifier();
        accept(Token.IS);

        TypeDenoter typeDenoter = parseTypeDenoter();

        accept(Token.SEMICOLON);
        finish(srcPos);

        return new TypeDefinition(srcPos, typeName, typeDenoter);
    }

    TypeDenoter parseTypeDenoter() throws SyntaxError {

        TypeDenoter typeDenoter = null;

        SourcePosition srcPos = new SourcePosition();
        start(srcPos);

        switch (currentToken.kind) {

            case Token.ARRAY:
                typeDenoter = parseArrayTypeDenoter();
                finish(srcPos);
                break;

            case Token.STRUCTURE:
                typeDenoter = parseStructureTypeDenoter();
                finish(srcPos);
                break;

            case Token.IDENTIFIER:
                typeDenoter = parseIdentifierTypeDenoter();
                finish(srcPos);
                break;

            default:
                syntacticError("\"%\" cannot start a type definition", currentToken.spelling);

        }

        return typeDenoter;
    }

    TypeDenoter parseArrayTypeDenoter() throws SyntaxError {
        SourcePosition srcPos = new SourcePosition();
        start(srcPos);

        accept(Token.ARRAY);
        ArrayBounds arrayBounds = parseArrayBounds();

        accept(Token.OF);

        TypeDenoter arrayType = parseTypeDenoter();

        finish(srcPos);
        return new ArrayType(srcPos, arrayBounds, arrayType);
    }


    ArrayBounds parseArrayBounds() throws SyntaxError {

        ArrayBounds arrayBounds;

        SourcePosition srcPos = new SourcePosition();
        start(srcPos);

        accept(Token.LBRACKET);

        Expression index = parseExpression();

        if (currentToken.kind == Token.COLON) {
            acceptIt();

            Expression to = parseExpression();

            finish(srcPos);
            arrayBounds = new SegmentedArrayBounds(srcPos, index, to);

        } else {
            finish(srcPos);
            arrayBounds = new SingleArrayBounds(srcPos, index);
        }

        accept(Token.RBRACKET);

        return arrayBounds;
    }

    TypeDenoter parseStructureTypeDenoter() throws SyntaxError {
        SourcePosition srcPos = new SourcePosition();
        start(srcPos);

        accept(Token.STRUCTURE);

        Field fieldDenoterList = parseFieldList();

        accept(Token.END);
        accept(Token.STRUCTURE);

        finish(srcPos);

        return new StructureType(srcPos, fieldDenoterList);
    }

    Field parseFieldList() throws SyntaxError {

        Field fieldDenoter;

        SourcePosition srcPos = new SourcePosition();
        start(srcPos);

        fieldDenoter = parseField();

        while (currentToken.kind == Token.COMMA) {
            acceptIt();

            FieldDenoter fieldDenoter2 = parseField();
            finish(srcPos);

            fieldDenoter = new FieldList(srcPos, fieldDenoter, fieldDenoter2);
        }

        return fieldDenoter;
    }

    FieldDenoter parseField() throws SyntaxError {
        SourcePosition srcPos = new SourcePosition();
        start(srcPos);

        accept(Token.FIELD);
        Identifier identifier = parseIdentifier();

        accept(Token.IS);
        TypeDenoter typeDenoter = parseTypeDenoter();

        return new FieldDenoter(srcPos, identifier, typeDenoter);
    }

    TypeDenoter parseIdentifierTypeDenoter() throws SyntaxError {
        SourcePosition srcPos = new SourcePosition();
        start(srcPos);

        Identifier identifier = parseIdentifier();
        finish(srcPos);

        return new IdentifierType(srcPos, identifier);
    }

//_____________________________________________________________________________
//                                                                Declarations

    Definition parseDeclaration() throws SyntaxError {
        SourcePosition srcPos = new SourcePosition();
        start(srcPos);

        accept(Token.DECLARE);

        DeclaredNames declaredNames = parseDeclaredNames();
        TypeDenoter typeDenoter = parseTypeDenoter();

        accept(Token.SEMICOLON);
        finish(srcPos);

        return new Declaration(srcPos, declaredNames, typeDenoter);
    }

    DeclaredNames parseDeclaredNames() throws SyntaxError {

        DeclaredNames declaredNames;

        SourcePosition srcPos = new SourcePosition();
        start(srcPos);

        if (currentToken.kind == Token.LPAREN) {
            declaredNames = parseMultipleDeclaredNames();
            finish(srcPos);

        } else {
            Identifier identifier = parseIdentifier();
            finish(srcPos);

            declaredNames = new SingleDeclaredName(srcPos, identifier);
        }

        return declaredNames;
    }

    DeclaredNames parseMultipleDeclaredNames() throws SyntaxError {

        SourcePosition srcPos = new SourcePosition();
        start(srcPos);

        accept(Token.LPAREN);

        DeclaredNames multipleDeclaredNames = parseDeclaredNamesSeq();

        accept(Token.RPAREN);
        finish(srcPos);

        return multipleDeclaredNames;
    }

    DeclaredNames parseDeclaredNamesSeq() throws SyntaxError {

        DeclaredNames declaredNames;

        SourcePosition srcPos = new SourcePosition();
        start(srcPos);

        declaredNames = parseDeclaredNames();

        while (currentToken.kind == Token.COMMA) {
            acceptIt();

            DeclaredNames declaredNames2 = parseDeclaredNames();
            finish(srcPos);

            declaredNames = new MultipleDeclaredNames(srcPos, declaredNames, declaredNames2);
        }

        return declaredNames;
    }

//_____________________________________________________________________________
//                                                             Not implemented

    private Definition parseInternalProcedure() {
        return null;
    }


//_____________________________________________________________________________
//             STATEMENTS has statement(extends segment) interface
//_____________________________________________________________________________

    Statement parseExecutableStatementSeq() throws SyntaxError {

        Statement statement = null;

        SourcePosition srcPos = new SourcePosition();
        start(srcPos);

        if (isStatement()) {

            statement = parseSingleExecutableStatement();
            finish(srcPos);

        } else {
            syntacticError("At least one statement should exist in program", "");
        }

        return statement;
    }

    Statement parseSingleExecutableStatement() throws SyntaxError {

        Statement statement;

        SourcePosition srcPos = new SourcePosition();
        start(srcPos);

        statement = parseStatement();
        finish(srcPos);

        while (isStatement()) {

            Statement statementNext = parseStatement();
            finish(srcPos);

            statement = new StatementSeq(srcPos, statement, statementNext);
        }

        return statement;
    }

    Statement parseStatement() throws SyntaxError {

        Statement statement;

        SourcePosition srcPos = new SourcePosition();
        start(srcPos);

        switch (currentToken.kind) {

            case Token.SET:
            case Token.CALL:
            case Token.RETURN:
            case Token.EXIT:
            case Token.IF:
            case Token.BEGIN:
            case Token.FOR:
            case Token.SELECT:
            case Token.REPEAT:
            case Token.REPENT:
            case Token.INPUT:
            case Token.OUTPUT:
                syntacticError("Not implemented. Use only semicolon!", "");

            case Token.SEMICOLON:
                acceptIt();

                statement = new NullStatement(srcPos);

                finish(srcPos);
                break;

            default:
                statement = new EmptyStatement(srcPos);
        }

        return statement;
    }

    private boolean isStatement() {
        int token = currentToken.kind;

        return token == Token.SET || token == Token.CALL || token == Token.RETURN || token == Token.EXIT ||
                token == Token.IF || token == Token.BEGIN || token == Token.FOR || token == Token.SELECT ||
                token == Token.REPEAT || token == Token.REPENT || token == Token.INPUT || token == Token.OUTPUT ||
                token == Token.SEMICOLON;
    }

//_____________________________________________________________________________
//                                                                  Expression

    // Dummy for now
    Expression parseExpression() throws SyntaxError {
        SourcePosition srcPos = new SourcePosition();
        start(srcPos);

        String expr = currentToken.spelling;

        acceptIt();
        finish(srcPos);

        return new ConstantExpression(srcPos, new Identifier(srcPos, expr));
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

                Statement eAST = parseExecutableStatementSeq();
                accept(Token.RBRACKET);
                finish(srcPos);

                vAST = new SubscriptVname(srcPos, vAST, eAST);
            }
        }

        return vAST;
    }
}
