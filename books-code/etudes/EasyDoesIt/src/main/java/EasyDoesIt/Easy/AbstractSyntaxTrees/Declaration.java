package EasyDoesIt.Easy.AbstractSyntaxTrees;

import EasyDoesIt.Easy.SyntacticAnalizer.SourcePosition;

public class Declaration extends Definition {

    public DeclaredNames declaredNames;
    public TypeDenoter typeDenoter;

    public Declaration(SourcePosition srcPos, DeclaredNames declaredNames, TypeDenoter typeDenoter) {
        super(srcPos);
        this.declaredNames = declaredNames;
        this.typeDenoter = typeDenoter;
    }

    @Override
    public Object visit(Visitor v, Object o) {
        return v.visitDeclaration(this, o);
    }
}