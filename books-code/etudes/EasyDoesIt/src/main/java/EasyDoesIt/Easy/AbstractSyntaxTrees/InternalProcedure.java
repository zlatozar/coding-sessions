package EasyDoesIt.Easy.AbstractSyntaxTrees;

import EasyDoesIt.Easy.SyntacticAnalizer.SourcePosition;

public class InternalProcedure extends Definition {

    public BlockCode blockCode;

    public InternalProcedure(SourcePosition srcPos, BlockCode blockCode) {
        super(srcPos);
        this.blockCode = blockCode;
    }

    @Override
    public Object visit(Visitor v, Object o) {
        return v.visitInternalProcedure(this, o);
    }
}
