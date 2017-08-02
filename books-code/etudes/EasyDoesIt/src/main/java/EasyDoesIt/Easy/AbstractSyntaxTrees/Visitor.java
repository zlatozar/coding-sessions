package EasyDoesIt.Easy.AbstractSyntaxTrees;

public interface Visitor {

//_____________________________________________________________________________
//                                                          Value or variables

    Object visitIdentifier(Identifier ast, Object o);

    Object visitDotVname(DotVname ast, Object o);
    Object visitSimpleVname(SimpleVname ast, Object o);
    Object visitSubscriptVname(SubscriptVname ast, Object o);

    Object visitVnameExpression(VnameExpression ast, Object o);
    Object visitConstantExpression(ConstantExpression ast, Object o);


//_____________________________________________________________________________
//                                                                     Program

    Object visitProgram(Program ast, Object o);
    Object visitProgramBody(ProgramBody ast, Object o);
    Object visitCommand(Segment ast, Object o);


//_____________________________________________________________________________
//                                                                  Statements

    Object visitEmptyStatement(EmptyStatement ast, Object o);
    Object visitNullStmt(NullStmt ast, Object o);
    Object visitStatementSeq(StatementSeq ast, Object o);



//_____________________________________________________________________________
//                                                                 Definitions

    Object visitDefinitionSeq(DefinitionSeq ast, Object o);
    Object visitEmptyDefinition(EmptyDefinition ast, Object o);

    Object visitTypeDefinition(TypeDefinition ast, Object o);
    Object visitIdentifierType(IdentifierType ast, Object o);
    Object visitArrayType(ArrayType ast, Object o);
    Object visitSingleArrayBounds(SingleArrayBounds ast, Object o);
    Object visitSegmentedArrayBounds(SegmentedArrayBounds ast, Object o);
    Object visitStructureType(StructureType ast, Object o);
    Object visitFieldList(FieldList ast, Object o);
    Object visitFieldDenoter(FieldDenoter ast, Object o);

    Object visitDeclaration(Declaration ast, Object o);
    Object visitSingleDeclaredName(SingleDeclaredName ast, Object o);
    Object visitMultipleDeclaredNames(MultipleDeclaredNames ast, Object o);
    Object visitEmptyDeclaredName(EmptyDeclaredName ast, Object o);

    Object visitInternalProcedure(InternalProcedure ast, Object o);
    Object visitProcedureDefinition(ProcedureDefinition ast, Object o);
    Object visitFunctionDefinition(FunctionDefinition ast, Object o);
    Object visitProcedureHead(ProcedureHead ast, Object o);
    Object visitProcedureEnd(ProcedureEnd ast, Object o);
    Object visitProcedureName(ProcedureName ast, Object o);
    Object visitParameterList(ParameterList ast, Object o);
    Object visitProgramWithParams(ProgramNameWithParams ast, Object o);
    Object visitParameterByValue(ParameterByValue ast, Object o);
    Object visitParameterByName(ParameterByName ast, Object o);
    Object visitFunctionHead(FunctionHead ast, Object o);
    Object visitFunctionEnd(FunctionEnd ast, Object o);

//_____________________________________________________________________________
//                                                                  Statements

    Object visitVariableList(VariableList ast, Object o);
    Object visitAssignmentStmt(AssignmentStmt ast, Object o);
    Object visitSingleVariable(SingleVariable ast, Object o);

    Object visitProcedureCallStmt(ProcedureCallStmt ast, Object o);
    Object visitCall(Call ast, Object o);
    Object visitExpressionList(ExpressionList ast, Object o);
    Object visitCallWithParams(CallWithParams ast, Object o);

    Object visitReturn(Return ast, Object o);

    Object visitReturnWithExpression(ReturnWithExpression ast, Object o);

    Object visitExitStmt(ExitStmt ast, Object o);
}
