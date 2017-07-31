package EasyDoesIt.Easy.AbstractSyntaxTrees;

import EasyDoesIt.Easy.SyntacticAnalizer.SourcePosition;

public class StructureType extends TypeDenoter {

    public Field fieldDenoterList;

    public StructureType(SourcePosition srcPos, Field fieldDenoterList) {
        super(srcPos);
        this.fieldDenoterList = fieldDenoterList;
    }

    @Override
    public Object visit(Visitor v, Object o) {
        return v.visitStructureType(this, o);
    }
}
