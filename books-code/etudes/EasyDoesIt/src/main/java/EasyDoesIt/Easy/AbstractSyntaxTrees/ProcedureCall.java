package EasyDoesIt.Easy.AbstractSyntaxTrees;

import EasyDoesIt.Easy.SyntacticAnalizer.SourcePosition;

public class ProcedureCall extends Statement {

    public ProcedureRef prcRef;

    public ProcedureCall(SourcePosition srcPos, ProcedureRef prcRef) {
        super(srcPos);
        this.prcRef = prcRef;
    }

    @Override
    public Object visit(Visitor v, Object o) {
        return v.visitProcedureRef(this, o);
    }
}
