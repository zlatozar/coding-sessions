package EasyDoesIt.Easy.AbstractSyntaxTrees;

import EasyDoesIt.Easy.SyntacticAnalizer.SourcePosition;

public class FieldList extends Field {

    Field fieldSeq;
    Field field;

    public FieldList(SourcePosition srcPos, Field fieldSeq, Field field) {
        super(srcPos);
        this.fieldSeq = fieldSeq;
        this.field = field;
    }

    @Override
    public Object visit(Visitor v, Object o) {
        return v.visitFieldList(this, o);
    }
}
