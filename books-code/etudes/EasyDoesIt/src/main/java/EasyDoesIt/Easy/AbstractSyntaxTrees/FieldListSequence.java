package EasyDoesIt.Easy.AbstractSyntaxTrees;

import EasyDoesIt.Easy.SyntacticAnalizer.SourcePosition;

public class FieldListSequence extends FieldList {

    public Field field1;
    public Field field2;

    public FieldListSequence(Field field1, Field field2, SourcePosition thePosition) {
        super(thePosition);
        this.field1 = field1;
        this.field2 = field2;
    }

    @Override
    public Object visit(Visitor v, Object o) {
        return v.visitFieldListSequence(this, o);
    }
}
