package EasyDoesIt.Easy.AbstractSyntaxTrees;

import EasyDoesIt.Easy.SyntacticAnalizer.SourcePosition;

public class ProgramEnd extends AST {

    public Identifier identifier;

    public ProgramEnd(Identifier iAST, SourcePosition thePosition) {
        super(thePosition);
        this.identifier = iAST;
    }

    @Override
    public Object visit(Visitor v, Object o) {
        return v.visitProgramEnd(this, o);
    }
}
