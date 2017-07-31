package EasyDoesIt.Easy.AbstractSyntaxTrees;

import EasyDoesIt.Easy.SyntacticAnalizer.SourcePosition;

public class VnameExpression extends Statement {

    public Vname V;

    public VnameExpression(SourcePosition srcPos, Vname vAST) {
        super(srcPos);

        this.V = vAST;
    }

    @Override
    public Object visit(Visitor v, Object o) {
        return v.visitVnameExpression(this, o);
    }
}
