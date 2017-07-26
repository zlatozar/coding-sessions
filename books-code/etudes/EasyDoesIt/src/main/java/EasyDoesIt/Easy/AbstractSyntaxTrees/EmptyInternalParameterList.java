package EasyDoesIt.Easy.AbstractSyntaxTrees;

import EasyDoesIt.Easy.SyntacticAnalizer.SourcePosition;

public class EmptyInternalParameterList extends Parameters {

    public EmptyInternalParameterList(SourcePosition thePosition) {
        super(thePosition);
    }

    @Override
    public Object visit(Visitor v, Object o) {
        return v.visitEmptyInternalParametersList(this, o);
    }
}
