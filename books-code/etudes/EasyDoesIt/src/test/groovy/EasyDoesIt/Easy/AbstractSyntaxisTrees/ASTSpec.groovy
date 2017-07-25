package EasyDoesIt.Easy.AbstractSyntaxisTrees

import spock.lang.Specification

class ASTSpec extends Specification {

    protected String createProgram(String statement) {
        return "PROGRAM simpleProgram:\n" +
               "$statement\n" +
               "END PROGRAM simple;"
    }
}