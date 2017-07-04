package PrintersDevil

/**
 * Here is all 15th commands:
 *
 * - ?papersize
 * - ?mode
 * - ?paragraph
 * - ?margin
 * - ?linespacing
 * - ?space
 * - ?blank
 * - ?center
 * - ?page
 * - ?testpage
 * - ?heading
 * - ?number
 * - ?break
 * - ?footnote
 * - ?alias
 */
class InterpretLine {

    private final Environment env = new Environment()

    private final InterpretCmd interpret

    String $PAPERSIZE   = '?papersize'
    String $MODE        = '?mode'
    String $PARAGRAPH   = '?paragraph'
    String $MARGIN      = '?margin'
    String $LINESPACING = '?linespacing'
    String $SPACE       = '?space'
    String $BLANK       = '?blank'
    String $CENTER      = '?center'
    String $PAGE        = '?page'
    String $TESTPAGE    = '?testpage'
    String $HEADING     = '?heading'
    String $NUMBER      = '?number'
    String $BREAK       = '?break'
    String $FOOTNOTE    = '?footnote'
    String $ALIAS       = '?alias'

    Set<String> ALL_COMMANDS = this.getProperties().findAll({ it -> it.getValue() instanceof String }).values()

    InterpretLine() {
        this.interpret = new InterpretCmd(env)
    }

    Environment getEnvironment() {
        return env
    }

    String process(String line) {

        if (!line.size()) {
            return ""
        }

        String[] possibleCmdParams = line.split(env.WORDS_SEP)
        String possibleCmd = possibleCmdParams[0]

        if (ALL_COMMANDS.contains(possibleCmd)) {
            interpretCmd(possibleCmdParams)

            return ""
        }

        return line
    }

    // Helper functions

    private void interpretCmd(String[] command) {

        String commandKeyWord = command[0]

        switch (commandKeyWord) {
            case $PAPERSIZE:
                println("${$PAPERSIZE}")
                interpret.$PAPERSIZE(command)
                break

            case $MODE:
                println("${$MODE}")
                interpret.$MODE(command)
                break

            case $PARAGRAPH:
                println("${$PARAGRAPH}")
                interpret.$PARAGRAPH(command)
                break

            case $MARGIN:
                println("${$MARGIN}")
                interpret.$MARGIN(command)
                break

            case $LINESPACING:
                println("${$LINESPACING}")
                interpret.$LINESPACING(command)
                break

            case $SPACE:
                println("${$SPACE}")
                interpret.$SPACE(command)
                break

            case $BLANK:
                println("${$BLANK}")
                interpret.$BLANK(command)
                break

            case $CENTER:
                println("${$CENTER}")
                interpret.$CENTER(command)
                break

            case $PAGE:
                println("${$PAGE}")
                interpret.$PAGE(command)
                break

            case $TESTPAGE:
                println("${$TESTPAGE}")
                interpret.$TESTPAGE(command)
                break

            case $HEADING:
                println("${$HEADING}")
                interpret.$HEADING(command)
                break

            case $NUMBER:
                println("${$NUMBER}")
                interpret.$NUMBER(command)
                break

            case $BREAK:
                println("${$BREAK}")
                interpret.$BREAK(command)
                break

            case $FOOTNOTE:
                println("${$FOOTNOTE}")
                interpret.$FOOTNOTE(command)
                break

            case $ALIAS:
                println("${$ALIAS}")
                interpret.$ALIAS(command)
                break

            // Indicates logical error
            default:
                throw new IllegalArgumentException("There is no defined interpretation for command: '$commandKeyWord'")
        }
    }

}
