package EasyDoesIt.Easy.AbstractSyntaxTrees;

import EasyDoesIt.Easy.SyntacticAnalizer.SourcePosition;

public abstract class BlockCode extends AST {

    public BlockCode(SourcePosition srcPos) {
        super(srcPos);
    }
}
