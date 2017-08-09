package EasyDoesIt.Easy.ContextualAnalizer;

import EasyDoesIt.Easy.AbstractSyntaxTrees.*;
import EasyDoesIt.Easy.ErrorReporter;
import EasyDoesIt.Easy.StdEnvironment;
import EasyDoesIt.Easy.SyntacticAnalizer.SourcePosition;

public final class Checker implements Visitor {

    // Always returns null. Does not use the given object.
    private static SourcePosition dummyPos = new SourcePosition();

    private final static Identifier dummyI = new Identifier(dummyPos, "");

    private IdentificationTable indTable;
    private ErrorReporter reporter;

    public Checker(ErrorReporter reporter) {
        this.reporter = reporter;
        this.indTable = new IdentificationTable();

        establishStdEnvironment();
    }

    // Checker entry point
    public void check(Program ast) {
        ast.visit(this, null);
    }

//_____________________________________________________________________________
//                                                          Value or variables

    @Override
    public Object visitIdentifier(Identifier ast, Object o) {
        return null;
    }

    @Override
    public Object visitDotVname(DotVname ast, Object o) {
        return null;
    }

    @Override
    public Object visitSimpleVname(SimpleVname ast, Object o) {
        return null;
    }

    @Override
    public Object visitSubscriptVname(SubscriptVname ast, Object o) {
        return null;
    }

//_____________________________________________________________________________
//                                                                 Expressions

    @Override
    public Object visitVnameExpression(VnameExpression ast, Object o) {
        return null;
    }

    @Override
    public Object visitConstantExpression(ConstantExpression ast, Object o) {
        return null;
    }

    @Override
    public Object visitBinaryExpression(BinaryExpression ast, Object o) {
        return null;
    }

    @Override
    public Object visitOperator(Operator ast, Object o) {
        return null;
    }

    @Override
    public Object visitCharacterLiteral(CharacterLiteral ast, Object o) {
        return null;
    }

    @Override
    public Object visitIntegerLiteral(IntegerLiteral ast, Object o) {
        return null;
    }

    @Override
    public Object visitUnaryExpression(UnaryExpression ast, Object o) {
        return null;
    }

    @Override
    public Object visitIntegerExpression(IntegerExpression ast, Object o) {
        return null;
    }

    @Override
    public Object visitFunctionCall(FunctionCall ast, Object o) {
        return null;
    }

    @Override
    public Object visitCharacterExpression(CharacterExpression ast, Object o) {
        return null;
    }

//_____________________________________________________________________________
//                                                                     Program

    @Override
    public Object visitProgram(Program ast, Object o) {
        return null;
    }

    @Override
    public Object visitProgramBody(ProgramBody ast, Object o) {
        return null;
    }

    @Override
    public Object visitCommand(Segment ast, Object o) {
        return null;
    }

//_____________________________________________________________________________
//                                                                 Definitions

    @Override
    public Object visitDefinitionSeq(DefinitionSeq ast, Object o) {
        return null;
    }

    @Override
    public Object visitEmptyDefinition(EmptyDefinition ast, Object o) {
        return null;
    }

    @Override
    public Object visitTypeDefinition(TypeDefinition ast, Object o) {
        return null;
    }

    @Override
    public Object visitIdentifierType(IdentifierType ast, Object o) {
        return null;
    }

    @Override
    public Object visitArrayType(ArrayType ast, Object o) {
        return null;
    }

    @Override
    public Object visitSingleArrayBounds(SingleArrayBounds ast, Object o) {
        return null;
    }

    @Override
    public Object visitSegmentedArrayBounds(SegmentedArrayBounds ast, Object o) {
        return null;
    }

    @Override
    public Object visitStructureType(StructureType ast, Object o) {
        return null;
    }

    @Override
    public Object visitFieldList(FieldList ast, Object o) {
        return null;
    }

    @Override
    public Object visitFieldDenoter(FieldDenoter ast, Object o) {
        return null;
    }

    @Override
    public Object visitDeclaration(Declaration ast, Object o) {
        return null;
    }

    @Override
    public Object visitSingleDeclaredName(SingleDeclaredName ast, Object o) {
        return null;
    }

    @Override
    public Object visitMultipleDeclaredNames(MultipleDeclaredNames ast, Object o) {
        return null;
    }

    @Override
    public Object visitEmptyDeclaredName(EmptyDeclaredName ast, Object o) {
        return null;
    }

    @Override
    public Object visitInternalProcedure(InternalProcedure ast, Object o) {
        return null;
    }

    @Override
    public Object visitProcedureDefinition(ProcedureDefinition ast, Object o) {
        return null;
    }

    @Override
    public Object visitFunctionDefinition(FunctionDefinition ast, Object o) {
        return null;
    }

    @Override
    public Object visitProcedureHead(ProcedureHead ast, Object o) {
        return null;
    }

    @Override
    public Object visitProcedureEnd(ProcedureEnd ast, Object o) {
        return null;
    }

    @Override
    public Object visitProcedureName(ProcedureName ast, Object o) {
        return null;
    }

    @Override
    public Object visitParameterList(ParameterList ast, Object o) {
        return null;
    }

    @Override
    public Object visitProcedureNameWithParams(ProcedureNameWithParams ast, Object o) {
        return null;
    }

    @Override
    public Object visitParameterByValue(ParameterByValue ast, Object o) {
        return null;
    }

    @Override
    public Object visitParameterByName(ParameterByName ast, Object o) {
        return null;
    }

    @Override
    public Object visitFunctionHead(FunctionHead ast, Object o) {
        return null;
    }

    @Override
    public Object visitFunctionEnd(FunctionEnd ast, Object o) {
        return null;
    }

//_____________________________________________________________________________
//                                                                  Statements

    @Override
    public Object visitEmptyStatement(EmptyStatement ast, Object o) {
        return null;
    }

    @Override
    public Object visitNullStmt(NullStmt ast, Object o) {
        return null;
    }

    @Override
    public Object visitStatementSeq(StatementSeq ast, Object o) {
        return null;
    }

    @Override
    public Object visitVariableList(VariableList ast, Object o) {
        return null;
    }

    @Override
    public Object visitAssignmentStmt(AssignmentStmt ast, Object o) {
        return null;
    }

    @Override
    public Object visitProcedureCallStmt(ProcedureCallStmt ast, Object o) {
        return null;
    }

    @Override
    public Object visitCall(Call ast, Object o) {
        return null;
    }

    @Override
    public Object visitExpressionList(ExpressionList ast, Object o) {
        return null;
    }

    @Override
    public Object visitCallWithParams(CallWithParams ast, Object o) {
        return null;
    }

    @Override
    public Object visitReturn(Return ast, Object o) {
        return null;
    }

    @Override
    public Object visitReturnWithExpression(ReturnWithExpression ast, Object o) {
        return null;
    }

    @Override
    public Object visitExitStmt(ExitStmt ast, Object o) {
        return null;
    }

    @Override
    public Object visitIfStmt(IfStmt ast, Object o) {
        return null;
    }

    @Override
    public Object visitIfElseStmt(IfElseStmt ast, Object o) {
        return null;
    }

    @Override
    public Object visitConditionalClause(ConditionalClause ast, Object o) {
        return null;
    }

    @Override
    public Object visitTrueBranch(TrueBranch ast, Object o) {
        return null;
    }

    @Override
    public Object visitFalseBranch(FalseBranch ast, Object o) {
        return null;
    }

    @Override
    public Object visitSimpleCompoundEnd(SimpleCompoundEnd ast, Object o) {
        return null;
    }

    @Override
    public Object visitCompoundNameWithName(CompoundEndWithName ast, Object o) {
        return null;
    }

    @Override
    public Object visitCompoundStmt(CompoundStmt ast, Object o) {
        return null;
    }

    @Override
    public Object visitForLoopStmt(ForLoopStmt ast, Object o) {
        return null;
    }

    @Override
    public Object visitForHead(ForHead ast, Object o) {
        return null;
    }

    @Override
    public Object visitWhile(While ast, Object o) {
        return null;
    }

    @Override
    public Object visitStepperWhile(StepperWhile ast, Object o) {
        return null;
    }

    @Override
    public Object visitStepper(Stepper ast, Object o) {
        return null;
    }

    @Override
    public Object visitStep(Step ast, Object o) {
        return null;
    }

    @Override
    public Object visitExpressionStep(ExpressionStep ast, Object o) {
        return null;
    }

    @Override
    public Object visitLimit(Limit ast, Object o) {
        return null;
    }

    @Override
    public Object visitExpressionStepLimit(ExpressionStepLimit ast, Object o) {
        return null;
    }

    @Override
    public Object visitExpressionLimit(ExpressionLimit ast, Object o) {
        return null;
    }

    @Override
    public Object visitSimpleForEnd(SimpleForEnd ast, Object o) {
        return null;
    }

    @Override
    public Object visitForEndWithName(ForEndWithName ast, Object o) {
        return null;
    }

    @Override
    public Object visitSelectionStmt(SelectionStmt ast, Object o) {
        return null;
    }

    @Override
    public Object visitSelectionHead(SelectionHead ast, Object o) {
        return null;
    }

    @Override
    public Object visitSelectBody(SelectBody ast, Object o) {
        return null;
    }

    @Override
    public Object visitSelectBodyWithEscape(SelectBodyWithEscape ast, Object o) {
        return null;
    }

    @Override
    public Object visitSelectionEnd(SelectEnd ast, Object o) {
        return null;
    }

    @Override
    public Object visitSelectEndWithName(SelectEndWithName ast, Object o) {
        return null;
    }

    @Override
    public Object visitCaseSeq(CaseSeq ast, Object o) {
        return null;
    }

    @Override
    public Object visitCaseHead(CaseHead ast, Object o) {
        return null;
    }

    @Override
    public Object visitCaseList(CaseList ast, Object o) {
        return null;
    }

    @Override
    public Object visitSelector(Selector ast, Object o) {
        return null;
    }

    @Override
    public Object visitEscapeCase(EscapeCase ast, Object o) {
        return null;
    }

    @Override
    public Object visitRepeat(Repeat ast, Object o) {
        return null;
    }

    @Override
    public Object visitRepent(Repent ast, Object o) {
        return null;
    }

    @Override
    public Object visitInput(Input ast, Object o) {
        return null;
    }

    @Override
    public Object visitInputList(InputList ast, Object o) {
        return null;
    }

    @Override
    public Object visitOutput(Output ast, Object o) {
        return null;
    }

    @Override
    public Object visitOutputList(OutputList ast, Object o) {
        return null;
    }

    @Override
    public Object visitSingleOutputExpression(SingleOutputExpression ast, Object o) {
        return null;
    }

//_____________________________________________________________________________
//                                                        Standard Environment

    @Override
    public Object visitUnaryOperatorDefinition(UnaryOperatorDefinition ast, Object o) {
        return null;
    }

    @Override
    public Object visitBinaryOperatorDefinition(BinaryOperatorDefinition ast, Object o) {
        return null;
    }

    @Override
    public Object visitAnyTypeDenoter(AnyTypeDenoter ast, Object o) {
        return null;
    }

    @Override
    public Object visitErrorTypeDenoter(ErrorTypeDenoter ast, Object o) {
        return null;
    }

    @Override
    public Object visitBoolTypeDenoter(BoolTypeDenoter ast, Object o) {
        return null;
    }

    @Override
    public Object visitIntTypeDenoter(IntTypeDenoter ast, Object o) {
        return null;
    }

    @Override
    public Object visitCharTypeDenoter(CharTypeDenoter ast, Object o) {
        return null;
    }

    @Override
    public Object visitFloatTypeDenoter(FloatTypeDenoter ast, Object o) {
        return null;
    }

    @Override
    public Object visitRealTypeDenoter(RealTypeDenoter ast, Object o) {
        return null;
    }

//_____________________________________________________________________________
//

    private void reportUndeclared(Terminal leaf) {
        reporter.reportError("\"%\" is not declared", leaf.spelling, leaf.position);
    }

    private TypeDefinition declareStdType(String id, TypeDenoter typedenoter) {

        TypeDefinition binding;

        binding = new TypeDefinition(dummyPos, new Identifier(dummyPos, id), typedenoter);
        indTable.enter(id, binding);

        return binding;
    }

    private AssignmentStmt declareStdConst(String id, TypeDenoter constType) {

        IntegerExpression constExpr;
        AssignmentStmt binding;

        // constExpr used only as a placeholder for constType
        constExpr = new IntegerExpression(dummyPos, null);
        constExpr.type = constType;

        binding = new AssignmentStmt(dummyPos, new SimpleVname(dummyPos, new Identifier(dummyPos, id)), constExpr);

        indTable.enter(id, binding);

        return binding;
    }

    private FunctionDefinition declareStdFunc(String id, Parameter fps, TypeDenoter resultType) {

        FunctionDefinition binding;

        Identifier name = new Identifier(dummyPos, id);
        BlockCodeName blockCodeName = new ProcedureNameWithParams(dummyPos, name, fps);

        FunctionHead funcHead = new FunctionHead(dummyPos, blockCodeName, resultType);
        Segment segment = new Segment(dummyPos, new EmptyDefinition(dummyPos), new EmptyStatement(dummyPos));
        FunctionEnd funcEnd = new FunctionEnd(dummyPos, name);

        binding = new FunctionDefinition(dummyPos, funcHead, segment, funcEnd);
        indTable.enter(id, binding);

        return binding;
    }

    private UnaryOperatorDefinition declareStdUnaryOp(String op, TypeDenoter argType, TypeDenoter resultType) {

        UnaryOperatorDefinition binding;

        binding = new UnaryOperatorDefinition(dummyPos, new Operator(dummyPos, op), argType, resultType);
        indTable.enter(op, binding);

        return binding;
    }

    private BinaryOperatorDefinition declareStdBinaryOp(String op, TypeDenoter arg1Type, TypeDenoter arg2type,
                                                        TypeDenoter resultType) {

        BinaryOperatorDefinition binding;

        binding = new BinaryOperatorDefinition(dummyPos, new Operator(dummyPos, op), arg1Type, arg2type, resultType);
        indTable.enter(op, binding);

        return binding;
    }

    private void establishStdEnvironment() {

        StdEnvironment.booleanType = new BoolTypeDenoter(dummyPos);
        StdEnvironment.charType = new CharTypeDenoter(dummyPos);
        StdEnvironment.integerType = new IntTypeDenoter(dummyPos);
        StdEnvironment.floatType = new FloatTypeDenoter(dummyPos);
        StdEnvironment.realType = new RealTypeDenoter(dummyPos);

        StdEnvironment.anyType = new AnyTypeDenoter(dummyPos);
        StdEnvironment.errorType = new ErrorTypeDenoter(dummyPos);

        StdEnvironment.booleanDecl = declareStdType("BOOLEAN", StdEnvironment.booleanType);
        StdEnvironment.falseDecl = declareStdConst("TRUE", StdEnvironment.booleanType);
        StdEnvironment.trueDecl = declareStdConst("FALSE", StdEnvironment.booleanType);
        StdEnvironment.notDecl = declareStdUnaryOp("NOT", StdEnvironment.booleanType, StdEnvironment.booleanType);
        StdEnvironment.andDecl = declareStdBinaryOp("AND", StdEnvironment.booleanType, StdEnvironment.booleanType, StdEnvironment.booleanType);
        StdEnvironment.orDecl = declareStdBinaryOp("OR", StdEnvironment.booleanType, StdEnvironment.booleanType, StdEnvironment.booleanType);

        StdEnvironment.integerDecl = declareStdType("INTEGER", StdEnvironment.integerType);
        StdEnvironment.moduloDecl = declareStdBinaryOp("MOD", StdEnvironment.integerType, StdEnvironment.integerType, StdEnvironment.integerType);
        StdEnvironment.lessDecl = declareStdBinaryOp("<", StdEnvironment.integerType, StdEnvironment.integerType, StdEnvironment.booleanType);
        StdEnvironment.greaterDecl = declareStdBinaryOp(">", StdEnvironment.integerType, StdEnvironment.integerType, StdEnvironment.booleanType);
        StdEnvironment.notlessDecl = declareStdBinaryOp(">=", StdEnvironment.integerType, StdEnvironment.integerType, StdEnvironment.booleanType);
        StdEnvironment.notgreaterDecl = declareStdBinaryOp("<=", StdEnvironment.integerType, StdEnvironment.integerType, StdEnvironment.booleanType);

        StdEnvironment.realDecl = declareStdType("REAL", StdEnvironment.realType);

        StdEnvironment.floatDecl = declareStdType("FLOAT", StdEnvironment.floatType);
        StdEnvironment.addDecl = declareStdBinaryOp("+", StdEnvironment.floatType, StdEnvironment.floatType, StdEnvironment.floatType);
        StdEnvironment.subtractDecl = declareStdBinaryOp("-", StdEnvironment.floatType, StdEnvironment.floatType, StdEnvironment.floatType);
        StdEnvironment.multiplyDecl = declareStdBinaryOp("*", StdEnvironment.floatType, StdEnvironment.floatType, StdEnvironment.floatType);
        StdEnvironment.divideDecl = declareStdBinaryOp("/", StdEnvironment.floatType, StdEnvironment.floatType, StdEnvironment.floatType);

        StdEnvironment.charDecl = declareStdType("CHARACTER", StdEnvironment.charType);

        StdEnvironment.substrDecl = declareStdFunc("SUBSTR",
                new ParameterList(dummyPos,
                        new ParameterList(dummyPos,
                                new ParameterByValue(dummyPos, dummyI, StdEnvironment.integerType),
                                new ParameterByValue(dummyPos, dummyI, StdEnvironment.integerType)),
                        new ParameterByValue(dummyPos, dummyI, StdEnvironment.integerType)), StdEnvironment.charType);

        StdEnvironment.lengthDecl = declareStdFunc("LENGTH", new ParameterByValue(dummyPos, dummyI, StdEnvironment.integerType), StdEnvironment.charType);
        StdEnvironment.concatDecl = declareStdBinaryOp("||", StdEnvironment.charType, StdEnvironment.charType, StdEnvironment.charType);

        StdEnvironment.equalDecl = declareStdBinaryOp("=", StdEnvironment.anyType, StdEnvironment.anyType, StdEnvironment.booleanType);
        StdEnvironment.unequalDecl = declareStdBinaryOp("<>", StdEnvironment.anyType, StdEnvironment.anyType, StdEnvironment.booleanType);
    }
}
