package PrintersDevil

class Environment {

    // Default values
    final String SPACE = " "
    final Set<String> SENTENCE_END = ['.', '?', '!', '.)', '?)', '!)', '."', '?"', '!"', '.")', '?")', '!")', ':']

    final int $papersise_HEIGHT = 40
    final int $papersise_WIDTH  = 72

    final String $mode_UNFILLED = 'unfilled'
    final String $mode_FILL     = 'fill'          // default
    final String $mode_JUSTIFY  = 'justify'

    final int $paragraph_INDENT = 3
    final int $paragraph_GAP    = 0

    int $margin_LEFT
    int $margin_RIGHT

    final int $linespacing_GAP = 1

    final int $space_N = 0

    final int $blank_N = 0

    int $testpage_N

    int $heading_DEPTH
    int $heading_PLACE
    int $heading_POSITION

    int $number_N
    int $footnote_DEPTH

    int $alias_FAKE
    int $alias_READL

    Set<String> $mode_ALL = this.getProperties().findAll({
        it -> it.getValue() instanceof String && it.getKey().startsWith('$mode')
    }).values()




}
