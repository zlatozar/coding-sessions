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
 *
 * ATTENTION: Passed lines should be trimmed
 */
class FormattingCmd {

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

    String[] process(String line) {

        if (!line.size()) {
            return []
        }

        String possibleCmd = line.split(' ')[0]

        if (ALL_COMMANDS.contains(possibleCmd)) {
            interpret(possibleCmd)

            return []
        }

        return possibleCmd
    }

    // Helper functions

    private void interpret(String[] command) {

        String commandKeyWord = command[0]

        switch (commandKeyWord) {
            case $PAPERSIZE:
                println("${$PAPERSIZE}")
                break

            case $MODE:
                println("${$MODE}")
                break

            case $PARAGRAPH:
                println("${$PARAGRAPH}")
                break

            case $MARGIN:
                println("${$MARGIN}")
                break

            case $LINESPACING:
                println("${$LINESPACING}")
                break

            case $SPACE:
                println("${$SPACE}")
                break

            case $BLANK:
                println("${$BLANK}")
                break

            case $CENTER:
                println("${$CENTER}")
                break

            case $PAGE:
                println("${$PAGE}")
                break

            case $TESTPAGE:
                println("${$TESTPAGE}")
                break

            case $HEADING:
                println("${$HEADING}")
                break

            case $NUMBER:
                println("${$NUMBER}")
                break

            case $BREAK:
                println("${$BREAK}")
                break

            case $FOOTNOTE:
                println("${$FOOTNOTE}")
                break

            case $ALIAS:
                println("${$ALIAS}")
                break

            // Indicates logical error
            default:
                throw new IllegalArgumentException("There is no defined interpretation for command: '$commandKeyWord'")
        }
    }

}
