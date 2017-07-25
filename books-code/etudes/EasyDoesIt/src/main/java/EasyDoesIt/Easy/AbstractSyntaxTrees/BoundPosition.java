package EasyDoesIt.Easy.AbstractSyntaxTrees;

import EasyDoesIt.Easy.SyntacticAnalizer.SourcePosition;

public class BoundPosition extends Bounds {

    public Expression position;

    public BoundPosition(Expression expression, SourcePosition thePosition) {
        super(thePosition);
        this.position = expression;
    }

    @Override
    public Object visit(Visitor v, Object o) {
        return v.visitBoundPosition(this, o);
    }


}
