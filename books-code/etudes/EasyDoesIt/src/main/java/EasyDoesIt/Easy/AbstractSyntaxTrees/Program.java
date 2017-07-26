package EasyDoesIt.Easy.AbstractSyntaxTrees;

import EasyDoesIt.Easy.SyntacticAnalizer.SourcePosition;

public class Program extends AST {

    public Segment S;

    public Program(SourcePosition srcPos, Segment sAST) {
        super(srcPos);

        this.S = sAST;
    }

    @Override
    public Object visit(Visitor v, Object o) {
        return v.visitProgram(this, o);
    }
}
