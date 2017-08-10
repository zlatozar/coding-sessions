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
    public Object visitIdentifier(Identifier I, Object o) {
        Definition binding = indTable.retrieve(I.spelling);

        if (binding != null) {
            I.decl = binding;
        }

        return binding;
    }

    public Object visitDotVname(DotVname ast, Object o) {
       return null;
    }

    @Override
    public Object visitSimpleVname(SimpleVname ast, Object o) {
        System.out.println("SimpleVname");
        return null;
    }

    @Override
    public Object visitSubscriptVname(SubscriptVname ast, Object o) {
        System.out.println("SubscriptVname");
        return null;
    }

//_____________________________________________________________________________
//                                                                 Expressions

    @Override
    public Object visitVnameExpression(VnameExpression ast, Object o) {
        System.out.println("VnameExpression");
        return null;
    }

    @Override
    public Object visitConstantExpression(ConstantExpression ast, Object o) {
        System.out.println("ConstantExpression");
        return null;
    }

    @Override
    public Object visitBinaryExpression(BinaryExpression ast, Object o) {
        System.out.println("BinaryExpression");
        return null;
    }

    @Override
    public Object visitOperator(Operator O, Object o) {

        Definition binding = indTable.retrieve(O.spelling);

        if (binding != null) {
            O.decl = binding;
        }

        return binding;
    }

    @Override
    public Object visitCharacterLiteral(CharacterLiteral ast, Object o) {
        System.out.println("CharacterLiteral");
        return null;
    }

    @Override
    public Object visitIntegerLiteral(IntegerLiteral ast, Object o) {
        System.out.println("IntegerLiteral");
        return null;
    }

    @Override
    public Object visitUnaryExpression(UnaryExpression ast, Object o) {
        System.out.println("UnaryExpression");
        return null;
    }

    @Override
    public Object visitIntegerExpression(IntegerExpression ast, Object o) {
        System.out.println("IntegerExpression");
        return null;
    }

    @Override
    public Object visitFunctionCall(FunctionCall ast, Object o) {
        System.out.println("FunctionCall");
        return null;
    }

    @Override
    public Object visitCharacterExpression(CharacterExpression ast, Object o) {
        System.out.println("CharacterExpression");
        return null;
    }

//_____________________________________________________________________________
//                                                                     Program

    @Override
    public Object visitProgram(Program ast, Object o) {
        System.out.println("Program");

        ast.program.visit(this, null);
        return null;
    }

    @Override
    public Object visitProgramBody(ProgramBody ast, Object o) {
        System.out.println("ProgramBody");

        indTable.openScope();

        ast.prgBody.visit(this, null);

        indTable.closeScope();

        return null;
    }

    @Override
    public Object visitCommand(Segment ast, Object o) {
        System.out.println("   Command");

        ast.definition.visit(this, null);
        ast.statement.visit(this, null);

        return null;
    }

//_____________________________________________________________________________
//                                                                 Definitions

    @Override
    public Object visitDefinitionSeq(DefinitionSeq ast, Object o) {
        indTable.openScope();

        // order matters
        ast.definitionSeq.visit(this, o);
        ast.definition.visit(this, o);

        indTable.closeScope();

        return null;
    }

    @Override
    public Object visitEmptyDefinition(EmptyDefinition ast, Object o) {
        System.out.println("EmptyDefinition");
        return null;
    }

    @Override
    public Object visitTypeDefinition(TypeDefinition ast, Object o) {
        ast.T = (TypeDenoter) ast.T.visit(this, null);
        indTable.enter(ast.I.spelling, ast);

        if (ast.duplicated) {
            reporter.reportError("identifier \"%\" already declared", ast.I.spelling, ast.position);
        }

        return null;
    }

    @Override
    public Object visitIdentifierType(IdentifierType ast, Object o) {
        System.out.println("IdentifierType");
        return null;
    }

    @Override
    public Object visitArrayType(ArrayType ast, Object o) {
        System.out.println("ArrayType");
        return null;
    }

    @Override
    public Object visitSingleArrayBounds(SingleArrayBounds ast, Object o) {
        System.out.println("SingleArrayBounds");
        return null;
    }

    @Override
    public Object visitSegmentedArrayBounds(SegmentedArrayBounds ast, Object o) {
        System.out.println("SegmentedArrayBounds");
        return null;
    }

    @Override
    public Object visitStructureType(StructureType ast, Object o) {
        System.out.println("      StructureType");

        indTable.openScope();

        ast.fieldDenoter = (Field) ast.fieldDenoter.visit(this, null);

        indTable.closeScope();

        return ast;
    }

    @Override
    public Object visitFieldList(FieldList ast, Object o) {

        ast.fieldSeq.visit(this, null);
        ast.field.visit(this, null);

        return null;
    }

    @Override
    public Object visitFieldDenoter(FieldDenoter ast, Object o) {

        return null;
    }

    @Override
    public Object visitDeclaration(Declaration ast, Object o) {
        System.out.println("Declaration");
        return null;
    }

    @Override
    public Object visitSingleDeclaredName(SingleDeclaredName ast, Object o) {
        System.out.println("SingleDeclaredName");
        return null;
    }

    @Override
    public Object visitMultipleDeclaredNames(MultipleDeclaredNames ast, Object o) {
        System.out.println("MultipleDeclaredNames");
        return null;
    }

    @Override
    public Object visitEmptyDeclaredName(EmptyDeclaredName ast, Object o) {
        System.out.println("EmptyDeclaredName");
        return null;
    }

    @Override
    public Object visitInternalProcedure(InternalProcedure ast, Object o) {
        System.out.println("InternalProcedure");
        return null;
    }

    @Override
    public Object visitProcedureDefinition(ProcedureDefinition ast, Object o) {
        System.out.println("ProcedureDefinition");
        return null;
    }

    @Override
    public Object visitFunctionDefinition(FunctionDefinition ast, Object o) {
        System.out.println("FunctionDefinition");
        return null;
    }

    @Override
    public Object visitProcedureHead(ProcedureHead ast, Object o) {
        System.out.println("ProcedureHead");
        return null;
    }

    @Override
    public Object visitProcedureEnd(ProcedureEnd ast, Object o) {
        System.out.println("ProcedureEnd");
        return null;
    }

    @Override
    public Object visitProcedureName(ProcedureName ast, Object o) {
        System.out.println("ProcedureName");
        return null;
    }

    @Override
    public Object visitParameterList(ParameterList ast, Object o) {
        System.out.println("ParameterList");
        return null;
    }

    @Override
    public Object visitProcedureNameWithParams(ProcedureNameWithParams ast, Object o) {
        System.out.println("ProcedureNameWithParams");
        return null;
    }

    @Override
    public Object visitParameterByValue(ParameterByValue ast, Object o) {
        System.out.println("ParameterByValue");
        return null;
    }

    @Override
    public Object visitParameterByName(ParameterByName ast, Object o) {
        System.out.println("ParameterByName");
        return null;
    }

    @Override
    public Object visitFunctionHead(FunctionHead ast, Object o) {
        System.out.println("FunctionHead");
        return null;
    }

    @Override
    public Object visitFunctionEnd(FunctionEnd ast, Object o) {
        System.out.println("FunctionEnd");
        return null;
    }

//_____________________________________________________________________________
//                                                                  Statements

    @Override
    public Object visitEmptyStatement(EmptyStatement ast, Object o) {
        System.out.println("EmptyStatement");
        return null;
    }

    @Override
    public Object visitNullStmt(NullStmt ast, Object o) {
        System.out.println("NullStmt");
        return null;
    }

    @Override
    public Object visitStatementSeq(StatementSeq ast, Object o) {
        System.out.println("StatementSeq");
        return null;
    }

    @Override
    public Object visitVariableList(VariableList ast, Object o) {
        System.out.println("VariableList");
        return null;
    }

    @Override
    public Object visitAssignmentStmt(AssignmentStmt ast, Object o) {
        System.out.println("AssignmentStmt");
        return null;
    }

    @Override
    public Object visitProcedureCallStmt(ProcedureCallStmt ast, Object o) {
        System.out.println("ProcedureCallStmt");
        return null;
    }

    @Override
    public Object visitCall(Call ast, Object o) {
        System.out.println("Call");
        return null;
    }

    @Override
    public Object visitExpressionList(ExpressionList ast, Object o) {
        System.out.println("ExpressionList");
        return null;
    }

    @Override
    public Object visitCallWithParams(CallWithParams ast, Object o) {
        System.out.println("CallWithParams");
        return null;
    }

    @Override
    public Object visitReturn(Return ast, Object o) {
        System.out.println("Return");
        return null;
    }

    @Override
    public Object visitReturnWithExpression(ReturnWithExpression ast, Object o) {
        System.out.println("ReturnWithExpression");
        return null;
    }

    @Override
    public Object visitExitStmt(ExitStmt ast, Object o) {
        System.out.println("ExitStmt");
        return null;
    }

    @Override
    public Object visitIfStmt(IfStmt ast, Object o) {
        System.out.println("IfStmt");
        return null;
    }

    @Override
    public Object visitIfElseStmt(IfElseStmt ast, Object o) {
        System.out.println("IfElseStmt");
        return null;
    }

    @Override
    public Object visitConditionalClause(ConditionalClause ast, Object o) {
        System.out.println("ConditionalClause");
        return null;
    }

    @Override
    public Object visitTrueBranch(TrueBranch ast, Object o) {
        System.out.println("TrueBranch");
        return null;
    }

    @Override
    public Object visitFalseBranch(FalseBranch ast, Object o) {
        System.out.println("FalseBranch");
        return null;
    }

    @Override
    public Object visitSimpleCompoundEnd(SimpleCompoundEnd ast, Object o) {
        System.out.println("SimpleCompoundEnd");
        return null;
    }

    @Override
    public Object visitCompoundNameWithName(CompoundEndWithName ast, Object o) {
        System.out.println("CompoundNameWithName");
        return null;
    }

    @Override
    public Object visitCompoundStmt(CompoundStmt ast, Object o) {
        System.out.println("CompoundStmt");
        return null;
    }

    @Override
    public Object visitForLoopStmt(ForLoopStmt ast, Object o) {
        System.out.println("ForLoopStmt");
        return null;
    }

    @Override
    public Object visitForHead(ForHead ast, Object o) {
        System.out.println("ForHead");
        return null;
    }

    @Override
    public Object visitWhile(While ast, Object o) {
        System.out.println("While");
        return null;
    }

    @Override
    public Object visitStepperWhile(StepperWhile ast, Object o) {
        System.out.println("StepperWhile");
        return null;
    }

    @Override
    public Object visitStepper(Stepper ast, Object o) {
        System.out.println("Stepper");
        return null;
    }

    @Override
    public Object visitStep(Step ast, Object o) {
        System.out.println("Step");
        return null;
    }

    @Override
    public Object visitExpressionStep(ExpressionStep ast, Object o) {
        System.out.println("ExpressionStep");
        return null;
    }

    @Override
    public Object visitLimit(Limit ast, Object o) {
        System.out.println("Limit");
        return null;
    }

    @Override
    public Object visitExpressionStepLimit(ExpressionStepLimit ast, Object o) {
        System.out.println("ExpressionStepLimit");
        return null;
    }

    @Override
    public Object visitExpressionLimit(ExpressionLimit ast, Object o) {
        System.out.println("ExpressionLimit");
        return null;
    }

    @Override
    public Object visitSimpleForEnd(SimpleForEnd ast, Object o) {
        System.out.println("SimpleForEnd");
        return null;
    }

    @Override
    public Object visitForEndWithName(ForEndWithName ast, Object o) {
        System.out.println("ForEndWithName");
        return null;
    }

    @Override
    public Object visitSelectionStmt(SelectionStmt ast, Object o) {
        System.out.println("SelectionStmt");
        return null;
    }

    @Override
    public Object visitSelectionHead(SelectionHead ast, Object o) {
        System.out.println("SelectionHead");
        return null;
    }

    @Override
    public Object visitSelectBody(SelectBody ast, Object o) {
        System.out.println("SelectBody");
        return null;
    }

    @Override
    public Object visitSelectBodyWithEscape(SelectBodyWithEscape ast, Object o) {
        System.out.println("SelectBodyWithEscape");
        return null;
    }

    @Override
    public Object visitSelectionEnd(SelectEnd ast, Object o) {
        System.out.println("SelectEnd");
        return null;
    }

    @Override
    public Object visitSelectEndWithName(SelectEndWithName ast, Object o) {
        System.out.println("SelectEndWithName");
        return null;
    }

    @Override
    public Object visitCaseSeq(CaseSeq ast, Object o) {
        System.out.println("CaseSeq");
        return null;
    }

    @Override
    public Object visitCaseHead(CaseHead ast, Object o) {
        System.out.println("CaseHead");
        return null;
    }

    @Override
    public Object visitCaseList(CaseList ast, Object o) {
        System.out.println("CaseList");
        return null;
    }

    @Override
    public Object visitSelector(Selector ast, Object o) {
        System.out.println("Selector");
        return null;
    }

    @Override
    public Object visitEscapeCase(EscapeCase ast, Object o) {
        System.out.println("EscapeCase");
        return null;
    }

    @Override
    public Object visitRepeat(Repeat ast, Object o) {
        System.out.println("Repeat");
        return null;
    }

    @Override
    public Object visitRepent(Repent ast, Object o) {
        System.out.println("Repent");
        return null;
    }

    @Override
    public Object visitInput(Input ast, Object o) {
        System.out.println("Input");
        return null;
    }

    @Override
    public Object visitInputList(InputList ast, Object o) {
        System.out.println("InputList");
        return null;
    }

    @Override
    public Object visitOutput(Output ast, Object o) {
        System.out.println("Output");
        return null;
    }

    @Override
    public Object visitOutputList(OutputList ast, Object o) {
        System.out.println("OutputList");
        return null;
    }

    @Override
    public Object visitSingleOutputExpression(SingleOutputExpression ast, Object o) {
        System.out.println("SingleOutputExpression");
        return null;
    }

//_____________________________________________________________________________
//                                                        Standard Environment

    @Override
    public Object visitUnaryOperatorDefinition(UnaryOperatorDefinition ast, Object o) {
        System.out.println("UnaryOperatorDefinition");
        return null;
    }

    @Override
    public Object visitBinaryOperatorDefinition(BinaryOperatorDefinition ast, Object o) {
        System.out.println("BinaryOperatorDefinition");
        return null;
    }

    @Override
    public Object visitAnyTypeDenoter(AnyTypeDenoter ast, Object o) {
        System.out.println("AnyTypeDenoter");
        return null;
    }

    @Override
    public Object visitErrorTypeDenoter(ErrorTypeDenoter ast, Object o) {
        System.out.println("ErrorTypeDenoter");
        return null;
    }

    @Override
    public Object visitBoolTypeDenoter(BoolTypeDenoter ast, Object o) {
        System.out.println("BoolTypeDenoter");
        return null;
    }

    @Override
    public Object visitIntTypeDenoter(IntTypeDenoter ast, Object o) {
        System.out.println("IntTypeDenoter");
        return null;
    }

    @Override
    public Object visitCharTypeDenoter(CharTypeDenoter ast, Object o) {
        System.out.println("CharTypeDenoter");
        return null;
    }

    @Override
    public Object visitFloatTypeDenoter(FloatTypeDenoter ast, Object o) {
        System.out.println("FloatTypeDenoter");
        return null;
    }

    @Override
    public Object visitRealTypeDenoter(RealTypeDenoter ast, Object o) {
        System.out.println("RealTypeDenoter");
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
                                new ParameterByValue(dummyPos, dummyI, StdEnvironment.charType),
                                new ParameterByValue(dummyPos, dummyI, StdEnvironment.integerType)),
                        new ParameterByValue(dummyPos, dummyI, StdEnvironment.integerType)), StdEnvironment.charType);

        StdEnvironment.lengthDecl = declareStdFunc("LENGTH", new ParameterByValue(dummyPos, dummyI, StdEnvironment.integerType), StdEnvironment.charType);
        StdEnvironment.concatDecl = declareStdBinaryOp("||", StdEnvironment.charType, StdEnvironment.charType, StdEnvironment.charType);

        StdEnvironment.equalDecl = declareStdBinaryOp("=", StdEnvironment.anyType, StdEnvironment.anyType, StdEnvironment.booleanType);
        StdEnvironment.unequalDecl = declareStdBinaryOp("<>", StdEnvironment.anyType, StdEnvironment.anyType, StdEnvironment.booleanType);
    }
}
