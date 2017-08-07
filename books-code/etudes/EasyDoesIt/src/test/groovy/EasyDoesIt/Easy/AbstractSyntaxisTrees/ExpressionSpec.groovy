package EasyDoesIt.Easy.AbstractSyntaxisTrees

import EasyDoesIt.Easy.AbstractSyntaxTrees.AST
import EasyDoesIt.Easy.SyntacticAnalizer.Parser

class ExpressionSpec extends ASTSpec {

    def 'Sting literal'() {

        given: 'String'
        Parser parser = getParserFor('OUTPUT "a < 0 in FUNCTION integersqrt."')

        when: 'Parser finish'

        then: 'AST should be constructed'
        AST theAST = parser.parseProgram();
        assert theAST
    }

    def 'Operation'() {

        given: 'Less'
        Parser parser = getParserFor(
                'SELECT select OF\n' +
                        '  CASE (select > 0):\n' +
                        '       DECLARE (x, ra) REAL;\n' +
                        '       DECLARE sqrt INTEGER;\n' +
                        '  ;' +
                        'END SELECT;')

        when: 'Parser finish'

        then: 'AST should be constructed'
        AST theAST = parser.parseProgram();
        assert theAST
    }

    def 'Negative'() {

        given: 'Unari operator'
        Parser parser = getParserFor('IF x < 0 THEN RETURN -x; ELSE RETURN x; FI;')

        when: 'Parser finish'

        then: 'AST should be constructed'
        AST theAST = parser.parseProgram();
        assert theAST
    }


}