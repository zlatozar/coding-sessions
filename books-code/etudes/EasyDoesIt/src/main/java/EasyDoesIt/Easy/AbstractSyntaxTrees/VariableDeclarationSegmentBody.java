package EasyDoesIt.Easy.AbstractSyntaxTrees;

import EasyDoesIt.Easy.SyntacticAnalizer.SourcePosition;

public class VariableDeclarationSegmentBody extends SegmentBody {

    public VariableDeclaration variableDeclaration;

    public VariableDeclarationSegmentBody(VariableDeclaration variableDeclaration, SourcePosition thePosition) {
        super(thePosition);
        this.variableDeclaration = variableDeclaration;
    }

    @Override
    public Object visit(Visitor v, Object o) {
        return v.visitVariableDeclarationSegmentBody(this, o);
    }
}
