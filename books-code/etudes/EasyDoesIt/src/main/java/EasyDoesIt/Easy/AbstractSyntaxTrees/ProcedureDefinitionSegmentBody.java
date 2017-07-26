package EasyDoesIt.Easy.AbstractSyntaxTrees;

import EasyDoesIt.Easy.SyntacticAnalizer.SourcePosition;

public class ProcedureDefinitionSegmentBody extends SegmentBody {

    public ProcedureDefinition procedureDefinition;

    public ProcedureDefinitionSegmentBody(ProcedureDefinition procedureDefinition, SourcePosition thePosition) {
        super(thePosition);
        this.procedureDefinition = procedureDefinition;
    }

    @Override
    public Object visit(Visitor v, Object o) {
        return v.visitProcedureDefinitionSegmentBody(this, o);
    }
}
