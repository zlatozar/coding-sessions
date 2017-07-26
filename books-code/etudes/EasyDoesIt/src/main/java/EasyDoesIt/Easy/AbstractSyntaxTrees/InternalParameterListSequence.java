package EasyDoesIt.Easy.AbstractSyntaxTrees;

import EasyDoesIt.Easy.SyntacticAnalizer.SourcePosition;

public class InternalParameterListSequence extends Parameters {

    public InternalParameter internalParameter1;
    public InternalParameter internalParameter2;

    public InternalParameterListSequence(InternalParameter internalParameter1, InternalParameter internalParameter2,
            SourcePosition thePosition) {
        super(thePosition);

        this.internalParameter1 = internalParameter1;
        this.internalParameter2 = internalParameter2;
    }

    @Override
    public Object visit(Visitor v, Object o) {
        return v.visitInternalParameterListSequence(this, o);
    }
}
