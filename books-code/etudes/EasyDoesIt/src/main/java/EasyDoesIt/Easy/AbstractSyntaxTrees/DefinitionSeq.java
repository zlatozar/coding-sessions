package EasyDoesIt.Easy.AbstractSyntaxTrees;

import EasyDoesIt.Easy.SyntacticAnalizer.SourcePosition;

public class DefinitionSeq extends Definition {

    public Definition definition;
    public Definition definitionSeq;

    public DefinitionSeq(SourcePosition srcPos, Definition definition, Definition definitionSeq) {
        super(srcPos);

        this.definition = definition;
        this.definitionSeq = definitionSeq;
    }

    @Override
    public Object visit(Visitor v, Object o) {
        return v.visitDefinitionSeq(this, o);
    }
}
