package EasyDoesIt.Easy.AbstractSyntaxTrees;

import EasyDoesIt.Easy.SyntacticAnalizer.SourcePosition;

public abstract class Definition extends AST {

    public Definition(SourcePosition srcPos) {
        super(srcPos);
    }
}
