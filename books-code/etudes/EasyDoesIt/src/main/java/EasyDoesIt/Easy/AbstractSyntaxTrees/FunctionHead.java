package EasyDoesIt.Easy.AbstractSyntaxTrees;

import EasyDoesIt.Easy.SyntacticAnalizer.SourcePosition;

public class FunctionHead extends AST {

    public ProcedureName procedureName;
    public Type type;

    public FunctionHead(ProcedureName procedureName, Type type, SourcePosition thePosition) {
        super(thePosition);

        this.procedureName = procedureName;
        this.type = type;
    }

    @Override
    public Object visit(Visitor v, Object o) {
        return v.visitFunctionHead(this, o);
    }
}
