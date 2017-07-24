package EasyDoesIt.Easy.AbstractSyntaxTrees;

import EasyDoesIt.Easy.SyntacticAnalizer.SourcePosition;

public class Identifier extends Terminal {

    public Identifier(String theSpelling, SourcePosition thePosition) {
        super(theSpelling, thePosition);
    }

    @Override
    public Object visit(Visitor v, Object o) {
        return v.visitIdentifier(this, o);
    }
}
