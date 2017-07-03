import PrintersDevil.Environment
import spock.lang.Specification
import spock.lang.Title

@Title('Environment settings')
class EnvironmentSpec extends Specification {

    def 'Check all valid modes'() {

        given: 'Environment with predefined settings'
        Environment env = new Environment()

        when: 'Request all valid modes'
        def modeALL = env.get$mode_ALL()

        then: 'All three should be returned'
        assert modeALL.size() == 3
        assert modeALL[0] == env.$mode_FILL
        assert modeALL[1] == env.$mode_UNFILLED
        assert modeALL[2] == env.$mode_JUSTIFY
    }

}