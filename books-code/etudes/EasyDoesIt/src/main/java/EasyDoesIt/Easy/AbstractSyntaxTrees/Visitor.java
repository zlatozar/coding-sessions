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
    Object visitCommand(Command ast, Object o);


//_____________________________________________________________________________
//                                                                  Statements

    Object visitEmptyStatement(EmptyStatement ast, Object o);
    Object visitNullStatement(NullStatement ast, Object o);
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



}
