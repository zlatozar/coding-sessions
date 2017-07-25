package EasyDoesIt.Easy.AbstractSyntaxTrees;

import EasyDoesIt.Easy.SyntacticAnalizer.SourcePosition;

public class TypeDefinitionSegmentBody extends SegmentBody {

    public TypeDefinition typeDefinition;

    public TypeDefinitionSegmentBody(TypeDefinition typeDefinition, SourcePosition thePosition) {
        super(thePosition);
        this.typeDefinition = typeDefinition;
    }

    @Override
    public Object visit(Visitor v, Object o) {
        return v.visitTypeDefinitionSegmentBody(this, o);
    }
}
