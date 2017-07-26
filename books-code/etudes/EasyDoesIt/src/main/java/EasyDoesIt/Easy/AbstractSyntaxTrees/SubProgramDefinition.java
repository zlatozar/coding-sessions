package EasyDoesIt.Easy.AbstractSyntaxTrees;

import EasyDoesIt.Easy.SyntacticAnalizer.SourcePosition;

public class SubProgramDefinition extends Functions {

    public SubProgramHead subProgramHead;
    public SegmentBody segmentBody;
    public SubProgramEnd subProgramEnd;

    public SubProgramDefinition(SubProgramHead subProgramHead, SegmentBody segmentBody,
                                SubProgramEnd subProgramEnd,
                                SourcePosition thePosition) {
        super(thePosition);
        this.subProgramHead = subProgramHead;
        this.segmentBody = segmentBody;
        this.subProgramEnd = subProgramEnd;
    }

    @Override
    public Object visit(Visitor v, Object o) {
        return v.visitSubProgramDefinition(this, o);
    }
}
