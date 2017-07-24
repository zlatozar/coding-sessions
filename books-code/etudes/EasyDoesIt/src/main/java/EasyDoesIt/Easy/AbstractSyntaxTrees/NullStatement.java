package EasyDoesIt.Easy.AbstractSyntaxTrees;

import EasyDoesIt.Easy.SyntacticAnalizer.SourcePosition;

public class NullStatement extends Terminal {

    public NullStatement(String theSpelling, SourcePosition thePosition) {
        super(theSpelling, thePosition);
    }

    @Override
    public Object visit(Visitor v, Object o) {
        return v.visitNullSegment(this, o);
    }
}
