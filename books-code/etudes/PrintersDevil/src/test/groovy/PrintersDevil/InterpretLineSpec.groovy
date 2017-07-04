package PrintersDevil

import spock.lang.Specification
import spock.lang.Title

@Title('How lines are interpreted')
class InterpretLineSpec extends Specification {

    private static final NUMBER_OF_DEFINED_CMD = 15

    private static final COMMANDS_FILE_NAME = 'src/test/resources/commands.txt'
    private int commandsNumber = 0

    def 'Discover commands'() {

        given: 'File that contains all commands'
        File commandsFile = new File(COMMANDS_FILE_NAME)
        assert commandsFile

        when: 'Reading file line by line (pass trimmed)'
        InterpretLine formattingCmd = new InterpretLine()

        commandsFile.eachLine({
            line ->
                def processed = []
                try {
                    processed = formattingCmd.process(line)

                } catch (Exception _) {
                    println("Error: $line")
                }

                if (processed.size() == 0) {
                    commandsNumber++
                }
        })

        then: 'All commands should be found'
        assert commandsNumber == NUMBER_OF_DEFINED_CMD

        and: 'All commands should be included'
        assert formattingCmd.ALL_COMMANDS.size() == 15
    }
}