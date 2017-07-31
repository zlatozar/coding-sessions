package EasyDoesIt.Easy.AbstractSyntaxTrees;

import EasyDoesIt.Easy.SyntacticAnalizer.SourcePosition;

public class FieldList extends Field {

    Field field;
    Field fieldSeq;

    public FieldList(SourcePosition srcPos, Field field, Field fieldSeq) {
        super(srcPos);
        this.field = field;
        this.fieldSeq = fieldSeq;
    }

    @Override
    public Object visit(Visitor v, Object o) {
        return v.visitFieldList(this, o);
    }
}
