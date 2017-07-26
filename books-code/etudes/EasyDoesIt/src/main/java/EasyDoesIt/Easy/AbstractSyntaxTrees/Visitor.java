package EasyDoesIt.Easy.AbstractSyntaxTrees;

public interface Visitor {

    //_____________________________________________________________________________
    //                                                                    Segments

    Object visitNullSegment(NullStatement ast, Object o);

    //_____________________________________________________________________________
    //                                                                 Expressions


    //_____________________________________________________________________________
    //                                                                Declarations

    Object visitVariableDeclaration(VariableDeclaration ast, Object o);

    Object visitDeclaredNames(DeclaredNames ast, Object o);

    Object visitDeclaredNamesSequence(DeclaredNamesSequence ast, Object o);

    //_____________________________________________________________________________
    //                                                            Array Aggregates

    Object visitBoundPosition(BoundPosition ast, Object o);

    Object visitBoundSection(BoundSection ast, Object o);

    //_____________________________________________________________________________
    //                                                           Record Aggregates

    //_____________________________________________________________________________
    //                                                                  Procedures

    Object visitFunctionDefinition(FunctionDefinition ast, Object o);

    Object visitFunctionHead(FunctionHead ast, Object o);

    Object visitFunctionEnd(FunctionEnd ast, Object o);

    Object visitProcedureDefinition(ProcedureDefinition ast, Object o);

    Object visitSubProgramDefinition(SubProgramDefinition ast, Object o);

    Object visitSubProgramHead(SubProgramHead ast, Object o);

    Object visitSubProgramEnd(SubProgramEnd ast, Object o);

    Object visitProcedureName(ProcedureName ast, Object o);

    Object visitEmptyInternalParametersList(EmptyInternalParameterList ast, Object o);

    Object visitInternalParameterList(InternalParameterList ast, Object o);

    Object visitInternalParameterListSequence(InternalParameterListSequence ast, Object o);

    Object visitPassByValue(PassByValue ast, Object o);

    Object visitPassByName(PassByName ast, Object o);

    //_____________________________________________________________________________
    //                                                               Type Denoters

    Object visitTypeDefinition(TypeDefinition ast, Object o);

    Object visitTypeIdentifier(TypeIdentifier ast, Object o);

    Object visitArrayedTypeDefinition(ArrayedTypeDefinition ast, Object o);

    Object visitStrictureType(StructureType ast, Object o);

    Object visitFieldListSingle(FieldListSingle ast, Object o);

    Object visitFieldListSequence(FieldListSequence ast, Object o);

    Object visitField(Field ast, Object o);

    //_____________________________________________________________________________
    //                                         Literals, Identifiers and Operators

    Object visitIdentifier(Identifier ast, Object o);

    //_____________________________________________________________________________
    //                                                     Value-or-variable names


    //_____________________________________________________________________________
    //                                                                    Programs

    Object visitProgram(Program ast, Object o);

    Object visitProgramHead(ProgramHead ast, Object o);

    Object visitNullSegmentBody(NullSegmentBody ast, Object o);

    Object visitTypeDefinitionSegmentBody(TypeDefinitionSegmentBody ast, Object o);

    Object visitTypeDefinitionSequenceSegmentBody(TypeDefinitionSequenceSegmentBody ast, Object o);

    Object visitVariableDeclarationSegmentBody(VariableDeclarationSegmentBody ast, Object o);

    Object visitVariableDeclarationSequenceSegmentBody(VariableDeclarationSequenceSegmentBody ast, Object o);

    Object visitProcedureDefinitionSegmentBody(ProcedureDefinitionSegmentBody ast, Object o);

    Object visitProcedureDefinitionSequenceSegmentBody(ProcedureDefinitionSequenceSegmentBody ast, Object o);

    Object visitProgramEnd(ProgramEnd ast, Object o);
}
