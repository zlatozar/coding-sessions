package EasyDoesIt.Easy.AbstractSyntaxTrees;

import EasyDoesIt.Easy.SyntacticAnalizer.SourcePosition;

public class MultipleDeclaredNames extends DeclaredNames {

    public DeclaredNames declaredNames;
    public DeclaredNames declaredNamesSeq;

    public MultipleDeclaredNames(SourcePosition srcPos, DeclaredNames declaredNames, DeclaredNames declaredNamesSeq) {
        super(srcPos);
        this.declaredNames = declaredNames;
        this.declaredNamesSeq = declaredNamesSeq;
    }

    @Override
    public Object visit(Visitor v, Object o) {
        return v.visitMultipleDeclaredNames(this, o);
    }
}
