package EasyDoesIt.Easy.AbstractSyntaxTrees;

import EasyDoesIt.Easy.SyntacticAnalizer.SourcePosition;

public class PassByValue extends InternalParameter {

    public Identifier identifier;
    public Type type;

    public PassByValue(Identifier identifier, Type type, SourcePosition thePosition) {
        super(thePosition);
        this.identifier = identifier;
        this.type = type;
    }

    @Override
    public Object visit(Visitor v, Object o) {
        return v.visitPassByValue(this, o);
    }
}
