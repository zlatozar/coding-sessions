package EasyDoesIt.Easy.AbstractSyntaxTrees;

import EasyDoesIt.Easy.SyntacticAnalizer.SourcePosition;

public class ProcedureName extends AST {

    public Identifier identifier;
    public Parameters parameters;

    public ProcedureName(Identifier identifier, Parameters parameters, SourcePosition thePosition) {
        super(thePosition);
        this.identifier = identifier;
        this.parameters = parameters;
    }

    @Override
    public Object visit(Visitor v, Object o) {
        return v.visitProcedureName(this, o);
    }
}
