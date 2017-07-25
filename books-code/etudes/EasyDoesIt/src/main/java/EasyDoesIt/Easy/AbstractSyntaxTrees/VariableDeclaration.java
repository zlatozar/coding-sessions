package EasyDoesIt.Easy.AbstractSyntaxTrees;

import EasyDoesIt.Easy.SyntacticAnalizer.SourcePosition;

public class VariableDeclaration extends AST {

    public Declarations declaration;
    public Type type;

    public VariableDeclaration(Declarations declaraiton, Type type, SourcePosition thePosition) {
        super(thePosition);
        this.declaration = declaraiton;
        this.type = type;
    }

    @Override
    public Object visit(Visitor v, Object o) {
        return v.visitVariableDeclaration(this, o);
    }
}
