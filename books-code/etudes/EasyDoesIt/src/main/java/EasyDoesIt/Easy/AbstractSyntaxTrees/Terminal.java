package EasyDoesIt.Easy.AbstractSyntaxTrees;

import EasyDoesIt.Easy.SyntacticAnalizer.SourcePosition;

abstract public class Terminal extends AST {

    // token spelling from scanner
    public String spelling;

    public Terminal(String theSpelling, SourcePosition thePosition) {
        super(thePosition);

        this.spelling = theSpelling;
    }
}
