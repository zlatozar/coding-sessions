package EasyDoesIt.Easy.AbstractSyntaxTrees;

import EasyDoesIt.Easy.SyntacticAnalizer.SourcePosition;

public class TypeDefinitionSequenceSegmentBody extends SegmentBody {

    // Sub type definitions
    TypeDefinition typeDefinition1;
    TypeDefinition typeDefinition2;

    public TypeDefinitionSequenceSegmentBody(TypeDefinition typeDefinition1, TypeDefinition typeDefinition2,
                                             SourcePosition thePosition) {
        super(thePosition);
        this.typeDefinition1 = typeDefinition1;
        this.typeDefinition2 = typeDefinition2;
    }

    @Override
    public Object visit(Visitor v, Object o) {
        return v.visitTypeDefinitionSequenceSegmentBody(this, o);
    }
}
