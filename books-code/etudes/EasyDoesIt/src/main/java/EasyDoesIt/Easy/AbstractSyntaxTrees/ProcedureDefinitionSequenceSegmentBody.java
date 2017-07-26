package EasyDoesIt.Easy.AbstractSyntaxTrees;

import EasyDoesIt.Easy.SyntacticAnalizer.SourcePosition;

public class ProcedureDefinitionSequenceSegmentBody extends SegmentBody {

    public ProcedureDefinition procedureDefinition1;
    public ProcedureDefinition procedureDefinition2;

    public ProcedureDefinitionSequenceSegmentBody(ProcedureDefinition procedureDefinition1,
                                                  ProcedureDefinition procedureDefinition2, SourcePosition thePosition) {
        super(thePosition);

        this.procedureDefinition1 = procedureDefinition1;
        this.procedureDefinition2 = procedureDefinition2;
    }

    @Override
    public Object visit(Visitor v, Object o) {
        return v.visitProcedureDefinitionSequenceSegmentBody(this, o);
    }
}
