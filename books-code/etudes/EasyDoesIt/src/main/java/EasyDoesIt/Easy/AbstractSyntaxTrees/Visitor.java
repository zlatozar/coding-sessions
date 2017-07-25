package EasyDoesIt.Easy.AbstractSyntaxTrees;

public interface Visitor {

    //_____________________________________________________________________________
    //                                                                    Segments

    Object visitNullSegment(NullStatement ast, Object o);

    //_____________________________________________________________________________
    //                                                                 Expressions


    //_____________________________________________________________________________
    //                                                                Declarations


    //_____________________________________________________________________________
    //                                                            Array Aggregates


    //_____________________________________________________________________________
    //                                                           Record Aggregates

    //_____________________________________________________________________________
    //                                                           Formal Parameters


    //_____________________________________________________________________________
    //                                                           Actual Parameters


    //_____________________________________________________________________________
    //                                                               Type Denoters

    Object visitTypeDefinition(TypeDefinition ast, Object o);

    Object visitTypeIdentifier(TypeIdentifier ast, Object o);

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

    Object visitProgramEnd(ProgramEnd ast, Object o);
}
