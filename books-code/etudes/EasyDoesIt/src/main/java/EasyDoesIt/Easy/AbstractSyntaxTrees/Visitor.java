package EasyDoesIt.Easy.AbstractSyntaxTrees;

public interface Visitor {

    Object visitProgram(Program ast, Object o);


    Object visitDotVname(DotVname ast, Object o);

    Object visitIdentifier(Identifier ast, Object o);


    Object visitSimpleVname(SimpleVname ast, Object o);

    Object visitSubscriptVname(SubscriptVname ast, Object o);


    Object visitTypeDeclaration(TypeDeclaration ast, Object o);


    Object visitVnameExpression(VnameExpression ast, Object o);
}
