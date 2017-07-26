package EasyDoesIt.Easy.AbstractSyntaxTrees;

import EasyDoesIt.Easy.SyntacticAnalizer.SourcePosition;

public class SubProgramEnd extends AST {

    public Identifier identifier;

    public SubProgramEnd(Identifier identifier, SourcePosition thePosition) {
        super(thePosition);
        this.identifier = identifier;
    }

    @Override
    public Object visit(Visitor v, Object o) {
        return v.visitSubProgramEnd(this, o);
    }
}
