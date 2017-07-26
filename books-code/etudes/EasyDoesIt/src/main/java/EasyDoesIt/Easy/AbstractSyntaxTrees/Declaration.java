package EasyDoesIt.Easy.AbstractSyntaxTrees;

import EasyDoesIt.Easy.SyntacticAnalizer.SourcePosition;

public abstract class Declaration extends AST {

    public boolean duplicated;

    public Declaration(SourcePosition srcPos) {
        super(srcPos);
        duplicated = false;
    }
}
