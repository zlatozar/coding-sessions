package EasyDoesIt.Easy.AbstractSyntaxTrees;

import EasyDoesIt.Easy.SyntacticAnalizer.SourcePosition;

/**
 * Each AST node has a tag that determines what (if any) subtrees that node has.
 */
public abstract class AST {

    public SourcePosition position;
    // public RuntimeEntity entity;

    public AST(SourcePosition thePosition) {
        this.position = thePosition;
        // this.entity = null;
    }

    public SourcePosition getPosition() {
        return position;
    }

    public abstract Object visit(Visitor v, Object o);
}
