package EasyDoesIt.Easy.AbstractSyntaxTrees;

import EasyDoesIt.Easy.SyntacticAnalizer.SourcePosition;

public class Program extends AST {

    public ProgramHead programHead;
    public SegmentBody segmentBody;
    public ProgramEnd programEnd;

    public Program(ProgramHead phAST, SegmentBody sbAST, ProgramEnd peAST, SourcePosition thePosition) {
        super(thePosition);

        this.programHead = phAST;
        this.segmentBody = sbAST;
        this.programEnd = peAST;
    }

    @Override
    public Object visit(Visitor v, Object o) {
        return v.visitProgram(this, o);
    }
}
