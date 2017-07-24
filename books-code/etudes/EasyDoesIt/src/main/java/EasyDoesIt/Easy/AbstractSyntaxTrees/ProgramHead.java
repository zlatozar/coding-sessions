package EasyDoesIt.Easy.AbstractSyntaxTrees;

import EasyDoesIt.Easy.SyntacticAnalizer.SourcePosition;

public class ProgramHead extends AST {

    public Identifier identifier;

    public ProgramHead(Identifier iAST, SourcePosition thePosition) {
        super(thePosition);

        this.identifier = iAST;
    }

    @Override
    public Object visit(Visitor v, Object o) {
        return v.visitProgramHead(this, o);
    }
}
