package EasyDoesIt.Easy.AbstractSyntaxTrees;

import EasyDoesIt.Easy.SyntacticAnalizer.SourcePosition;

public class TypeDeclaration extends Declaration {

    public Identifier I;
    public TypeDenoter T;

    public TypeDeclaration(SourcePosition srcPos, Identifier iAST, TypeDenoter tAST) {
        super(srcPos);

        this.I = iAST;
        this.T = tAST;
    }

    @Override
    public Object visit(Visitor v, Object o) {
        return v.visitTypeDeclaration(this, o);
    }
}
