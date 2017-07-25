package EasyDoesIt.Easy.AbstractSyntaxTrees;

import EasyDoesIt.Easy.SyntacticAnalizer.SourcePosition;

public class DeclaredNames extends Declarations {

    public Identifier identifier;

    public DeclaredNames(Identifier identifier, SourcePosition thePosition) {
        super(thePosition);
        this.identifier = identifier;
    }

    @Override
    public Object visit(Visitor v, Object o) {
        return v.visitDeclaredNames(this, o);
    }
}
