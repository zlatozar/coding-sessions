package EasyDoesIt.Easy.AbstractSyntaxTrees;

import EasyDoesIt.Easy.SyntacticAnalizer.SourcePosition;

public class ArrayType extends TypeDenoter {

    public ArrayBounds arrayBounds;
    public TypeDenoter type;

    public ArrayType(SourcePosition srcPos, ArrayBounds arrayBounds, TypeDenoter type) {
        super(srcPos);
        this.arrayBounds = arrayBounds;
        this.type = type;
    }

    @Override
    public Object visit(Visitor v, Object o) {
        return v.visitArrayType(this, o);
    }
}
