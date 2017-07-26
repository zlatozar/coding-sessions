package EasyDoesIt.Easy.AbstractSyntaxTrees;

import EasyDoesIt.Easy.SyntacticAnalizer.SourcePosition;

public class InternalParameterList extends Parameters {

    public InternalParameter internalParameter;

    public InternalParameterList(InternalParameter internalParameter, SourcePosition thePosition) {
        super(thePosition);
        this.internalParameter = internalParameter;
    }

    @Override
    public Object visit(Visitor v, Object o) {
        return v.visitInternalParameterList(this, o);
    }
}
