package EasyDoesIt.Easy.AbstractSyntaxTrees;

import EasyDoesIt.Easy.SyntacticAnalizer.SourcePosition;

public class SubProgramHead extends AST {

    public ProcedureName procedureName;

    public SubProgramHead(ProcedureName procedureName, SourcePosition thePosition) {
        super(thePosition);
        this.procedureName = procedureName;
    }

    @Override
    public Object visit(Visitor v, Object o) {
        return v.visitSubProgramHead(this, o);
    }
}
