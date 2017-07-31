package EasyDoesIt.Easy.AbstractSyntaxTrees;

import EasyDoesIt.Easy.SyntacticAnalizer.SourcePosition;

public class FieldDenoter extends Field {

    public Identifier I;
    public TypeDenoter typeDenoter;

    public FieldDenoter(SourcePosition srcPos, Identifier i, TypeDenoter typeDenoter) {
        super(srcPos);
        I = i;
        this.typeDenoter = typeDenoter;
    }

    @Override
    public Object visit(Visitor v, Object o) {
        return v.visitFieldDenoter(this, o);
    }
}
