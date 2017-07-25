package EasyDoesIt.Easy.AbstractSyntaxTrees;

import EasyDoesIt.Easy.SyntacticAnalizer.SourcePosition;

public class TypeIdentifier extends Type {

    public Identifier identifier;

    public TypeIdentifier(Identifier identifier, SourcePosition thePosition) {
        super(thePosition);
        this.identifier = identifier;
    }

    @Override
    public Object visit(Visitor v, Object o) {
        return v.visitTypeIdentifier(this, o);
    }
}
