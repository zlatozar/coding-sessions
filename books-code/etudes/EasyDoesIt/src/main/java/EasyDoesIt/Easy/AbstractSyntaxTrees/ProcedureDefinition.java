package EasyDoesIt.Easy.AbstractSyntaxTrees;

import EasyDoesIt.Easy.SyntacticAnalizer.SourcePosition;

public class ProcedureDefinition extends AST {

    public Functions function;

    public ProcedureDefinition(Functions function, SourcePosition thePosition) {
        super(thePosition);
        this.function = function;
    }

    @Override
    public Object visit(Visitor v, Object o) {
        return v.visitProcedureDefinition(this, o);
    }
}
