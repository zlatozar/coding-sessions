package EasyDoesIt.Easy.AbstractSyntaxTrees;

import EasyDoesIt.Easy.SyntacticAnalizer.SourcePosition;

public class FieldListSingle extends FieldList {

    public Field field;

    public FieldListSingle(Field field, SourcePosition thePosition) {
        super(thePosition);
        this.field = field;
    }

    @Override
    public Object visit(Visitor v, Object o) {
        return v.visitFieldListSingle(this, o);
    }
}
