package EasyDoesIt.Easy.AbstractSyntaxTrees;

import EasyDoesIt.Easy.SyntacticAnalizer.SourcePosition;

public class SegmentBody extends AST {

    public NullStatement nullStatement;

    public SegmentBody(NullStatement nsAST, SourcePosition thePosition) {
        super(thePosition);

        this.nullStatement = nsAST;
    }

    @Override
    public Object visit(Visitor v, Object o) {
        return v.visitSegmentBody(this, o);
    }
}
