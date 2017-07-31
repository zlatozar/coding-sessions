package EasyDoesIt.Easy.AbstractSyntaxTrees;

import EasyDoesIt.Easy.SyntacticAnalizer.SourcePosition;

public class EmptyDeclaredName extends DeclaredNames {

    public EmptyDeclaredName(SourcePosition srcPos) {
        super(srcPos);
    }

    @Override
    public Object visit(Visitor v, Object o) {
        return v.visitEmptyDeclaredName(this, o);
    }
}
