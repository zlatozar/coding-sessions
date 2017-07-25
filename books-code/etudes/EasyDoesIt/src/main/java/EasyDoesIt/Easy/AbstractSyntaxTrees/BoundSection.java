package EasyDoesIt.Easy.AbstractSyntaxTrees;

import EasyDoesIt.Easy.SyntacticAnalizer.SourcePosition;

public class BoundSection extends Bounds {

    public Expression lowerPosition;
    public Expression upperPosition;

    public BoundSection(Expression expression1, Expression expression2, SourcePosition thePosition) {
        super(thePosition);
        this.lowerPosition = expression1;
        this.upperPosition = expression2;
    }

    @Override
    public Object visit(Visitor v, Object o) {
        return v.visitBoundSection(this, o);
    }
}
