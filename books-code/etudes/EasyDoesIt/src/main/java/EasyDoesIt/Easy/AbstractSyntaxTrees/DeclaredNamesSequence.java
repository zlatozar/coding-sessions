package EasyDoesIt.Easy.AbstractSyntaxTrees;

import EasyDoesIt.Easy.SyntacticAnalizer.SourcePosition;

public class DeclaredNamesSequence extends Declarations {

    public Identifier identifier1;
    public Identifier identifier2;

    public DeclaredNamesSequence(Identifier identifier1, Identifier identifier2, SourcePosition thePosition) {
        super(thePosition);
        this.identifier1 = identifier1;
        this.identifier2 = identifier2;
    }

    @Override
    public Object visit(Visitor v, Object o) {
        return v.visitDeclaredNamesSequence(this, o);
    }
}
