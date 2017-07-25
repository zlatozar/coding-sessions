package EasyDoesIt.Easy.AbstractSyntaxTrees;

import EasyDoesIt.Easy.SyntacticAnalizer.SourcePosition;

public class VariableDeclarationSequenceSegmentBody extends SegmentBody {

    public VariableDeclaration variableDeclaration1;
    public VariableDeclaration variableDeclaration2;

    public VariableDeclarationSequenceSegmentBody(VariableDeclaration variableDeclaration1,
                                                  VariableDeclaration variableDeclaration2,
                                                  SourcePosition thePosition) {
        super(thePosition);
        this.variableDeclaration1 = variableDeclaration1;
        this.variableDeclaration2 = variableDeclaration2;
    }

    @Override
    public Object visit(Visitor v, Object o) {
        return v.visitVariableDeclarationSequenceSegmentBody(this, o);
    }
}
