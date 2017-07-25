package EasyDoesIt.Easy.AbstractSyntaxTrees;

import EasyDoesIt.Easy.SyntacticAnalizer.SourcePosition;

public class NullSegmentBody extends SegmentBody {

    public NullStatement nullStatement;

    public NullSegmentBody(NullStatement nsAST, SourcePosition thePosition) {
        super(thePosition);

        this.nullStatement = nsAST;
    }

    @Override
    public Object visit(Visitor v, Object o) {
        return v.visitNullSegmentBody(this, o);
    }
}
