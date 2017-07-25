package EasyDoesIt.Easy.AbstractSyntaxTrees;

import EasyDoesIt.Easy.SyntacticAnalizer.SourcePosition;

public class StructureType extends Type {

    public FieldList fieldList;

    public StructureType(FieldList fieldList, SourcePosition thePosition) {
        super(thePosition);
        this.fieldList = fieldList;
    }

    @Override
    public Object visit(Visitor v, Object o) {
        return v.visitStrictureType(this, o);
    }
}
