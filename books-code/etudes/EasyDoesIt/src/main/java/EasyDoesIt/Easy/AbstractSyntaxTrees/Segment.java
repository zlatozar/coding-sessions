package EasyDoesIt.Easy.AbstractSyntaxTrees;

import EasyDoesIt.Easy.SyntacticAnalizer.SourcePosition;

public abstract class Segment extends AST {

    public Segment(SourcePosition srcPos) {
        super(srcPos);
    }
}
