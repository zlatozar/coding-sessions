package EasyDoesIt.Easy.AbstractSyntaxTrees;

import EasyDoesIt.Easy.SyntacticAnalizer.SourcePosition;

public class TypeDefinition extends AST {

    public Identifier identifier;
    public Type type;

    public TypeDefinition(Identifier identifier, Type type, SourcePosition thePosition) {
        super(thePosition);

        this.identifier = identifier;
        this.type = type;
    }

    @Override
    public Object visit(Visitor v, Object o) {
        return v.visitTypeDefinition(this, o);
    }
}
