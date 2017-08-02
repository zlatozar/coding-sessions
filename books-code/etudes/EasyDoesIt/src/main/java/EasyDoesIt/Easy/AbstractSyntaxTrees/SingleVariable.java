package EasyDoesIt.Easy.AbstractSyntaxTrees;

import EasyDoesIt.Easy.SyntacticAnalizer.SourcePosition;

public class SingleVariable extends Vname {

    public Vname var;

    public SingleVariable(SourcePosition srcPos, Vname var) {
        super(srcPos);
        this.var = var;
    }

    @Override
    public Object visit(Visitor v, Object o) {
        return v.visitSingleVariable(this, o);
    }
}
