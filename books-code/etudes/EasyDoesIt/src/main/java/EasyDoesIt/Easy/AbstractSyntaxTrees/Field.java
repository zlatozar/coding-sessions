package EasyDoesIt.Easy.AbstractSyntaxTrees;

import EasyDoesIt.Easy.SyntacticAnalizer.SourcePosition;

public class Field extends AST {

    public Identifier identifier;
    public Type type;

    public Field(Identifier identifier, Type type, SourcePosition thePosition) {
        super(thePosition);
        this.identifier = identifier;
        this.type = type;
    }

    @Override
    public Object visit(Visitor v, Object o) {
        return v.visitField(this, o);
    }
}
