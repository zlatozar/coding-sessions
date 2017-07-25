package EasyDoesIt.Easy.AbstractSyntaxTrees;

import EasyDoesIt.Easy.SyntacticAnalizer.SourcePosition;

public class ArrayedTypeDefinition extends Type {

    public Bounds bounds;
    public Type type;


    public ArrayedTypeDefinition(Bounds bounds, Type type, SourcePosition thePosition)  {
        super(thePosition);
        this.bounds = bounds;
        this.type = type;
    }

    @Override
    public Object visit(Visitor v, Object o) {
        return v.visitArrayedTypeDefinition(this, o);
    }
}
