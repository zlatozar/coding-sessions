package EasyDoesIt.Easy.AbstractSyntaxTrees;

import EasyDoesIt.Easy.SyntacticAnalizer.SourcePosition;

public class FunctionDefinition extends Functions {

    public FunctionHead functionHead;
    public SegmentBody segmentBody;
    public FunctionEnd functionEnd;

    public FunctionDefinition(FunctionHead functionHead, SegmentBody segmentBody,
                              FunctionEnd functionEnd, SourcePosition thePosition) {
        super(thePosition);

        this.functionHead = functionHead;
        this.segmentBody = segmentBody;
        this.functionEnd = functionEnd;
    }

    @Override
    public Object visit(Visitor v, Object o) {
        return v.visitFunctionDefinition(this, o);
    }
}
