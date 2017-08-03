package EasyDoesIt.Easy.AbstractSyntaxTrees;

import EasyDoesIt.Easy.SyntacticAnalizer.SourcePosition;

public class SingleOutputExpression extends Expression {

    public Expression expr;

    public SingleOutputExpression(SourcePosition srcPos, Expression expr) {
        super(srcPos);
        this.expr = expr;
    }

    @Override
    public Object visit(Visitor v, Object o) {
        return v.visitSingleOutputExpression(this, o);
    }
}
