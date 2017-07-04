package PrintersDevil

class Environment extends Constants {

    // Keeps track of the current line in source file
    private int lineNumber = 0

    // Formatting commands
    private boolean shouldBreak = false
    private int $papersise_HEIGHT = 40
    private int $papersise_WIDTH  = 72
    private String $mode          = FILL_mode
    private int $paragraph_INDENT = 3
    private int $paragraph_GAP    = 0
    private int $margin_LEFT      = 0
    private int $margin_RIGHT     = 72
    private int $linespacing_GAP  = 1
    private int $space_N          = 0
    private int $blank_N          = 0
    private boolean $center       = false
    private boolean $page         = false
    private int $testpage_N
    private int $heading_DEPTH    = 0
    private String $heading_PLACE = ''  // left, right or center
    private int $heading_POSITION = 0
    private int $number_N         = 0
    private int $footnote_DEPTH   = 0

    // real:fake
    private Map<String, String> $alias_MAP = [:]

    // It is possible BLANK(aka space used as word separator) to be replaced
    private String wordSeparator = $alias_MAP.get(WORDS_SEP) ? $alias_MAP.get(WORDS_SEP) : WORDS_SEP

    int getLineNumber() {
        return lineNumber
    }

    void setLineNumber(int number) {
        this.lineNumber = number
    }

    boolean breakParagraph() {
        return shouldBreak
    }

    void setParagraphBreak(boolean shouldBreak) {
        this.shouldBreak = shouldBreak
    }

    int getPapersizeHeight() {
        return $papersise_HEIGHT
    }

    int getPapersizeWidth() {
        return $papersise_WIDTH
    }

    void setPapersize(int height, int width) {
        this.$papersise_HEIGHT = height
        this.$papersise_WIDTH = width

        this.$margin_RIGHT = width
    }

    String getParagraphMode() {
        return $mode
    }

    void setParagraphMode(String mode) {
        this.$mode = mode
    }

    int getParagraphIndent() {
        return $paragraph_INDENT
    }

    int getParagraphGap() {
        return $paragraph_GAP
    }

    void setParagraph(int indent, int gap) {
        this.$paragraph_INDENT = indent
        this.$paragraph_GAP = gap
    }

    int getMarginLeft() {
        return $margin_LEFT
    }

    int getMarginRight() {
        return $margin_RIGHT
    }

    void setMargin(int left, int right) {
        this.$margin_LEFT = left
        this.$margin_RIGHT = right
    }

    int getLinespacingGap() {
        return $linespacing_GAP
    }

    void setLinespacingGap(int linespacingGap) {
        this.$linespacing_GAP = linespacingGap
    }

    int getSpace_N() {
        return $space_N
    }

    void setSpace_N(int n) {
        this.$space_N = n
    }

    int getBlank_N() {
        return $blank_N
    }

    void setBlank_N(int n) {
        this.$blank_N = n
    }

    boolean toCenter() {
        return $center
    }

    void setCenter(boolean center) {
        this.$center = center
    }

    boolean toNewPage() {
        return $page
    }

    void setNewPage(boolean page) {
        this.$page = page
    }

    int getTestpage_N() {
        return $testpage_N
    }

    void setTestpage_N(int n) {
        this.$testpage_N = n
    }

    int getHeadingDepth() {
        return $heading_DEPTH
    }

    String getHeadingPlace() {
        return $heading_PLACE
    }

    int getHeadingPosition() {
        return $heading_POSITION
    }

    void setHeading(int depth, String place, int position) {
        this.$heading_DEPTH = depth
        this.$heading_PLACE = place
        this.$heading_POSITION = position
    }

    int getPageNumber() {
        return $number_N
    }

    void setPageNumber(int n) {
        this.$number_N = n
    }

    int getFootnoteDepth() {
        return $footnote_DEPTH
    }

    void setFootnoteDepth(int depth) {
        this.$footnote_DEPTH = depth
    }

    Map<String, String> getAliases() {
        return $alias_MAP
    }

    void setAlias(String real, String fake) {
        this.$alias_MAP[real] = fake
    }

    void resetAliases() {
        this.$alias_MAP = [:]
    }

    String getAliasFor(String key) {
        this.$alias_MAP[key]
    }

    String getWordSeparator() {
        return wordSeparator
    }

    void setWordSeparator(String wordSeparator) {
        this.wordSeparator = wordSeparator
    }

    int getTextStartPosition() {
        return getMarginLeft()
    }

    int getParagraphFirstLineStartPosition() {
        return getMarginLeft() + getParagraphIndent()
    }

    int getTextWidth() {
        int rightOutdent = getPapersizeWidth() - getMarginRight()
        return getPapersizeWidth() - getMarginLeft() - rightOutdent
    }

    @Override
    String toString() {
        return "Environment {\n" +
                "     ?papersize ${getPapersizeHeight()} ${getPapersizeWidth()}\n" +
                "     ?mode ${getParagraphMode()}\n" +
                "     ?paragraph ${getParagraphIndent()} ${getParagraphGap()}\n" +
                "     ?margin ${getMarginLeft()}, ${getMarginRight()}\n" +
                "     ?linespacing ${getLinespacingGap()}\n" +
                "     ?space ${getSpace_N()}\n" +
                "     ?blank ${getBlank_N()}\n" +
                "     ?center ${toCenter()}\n" +
                "     ?page ${toNewPage()}\n" +
                "     ?testpage ${getTestpage_N()}\n" +
                "     ?heading ${getHeadingDepth()} ${getHeadingPlace()} ${getHeadingPosition()}\n" +
                "     ?number ${getPageNumber()}\n" +
                "     Should break? ${breakParagraph()}\n" +
                "     ?footnote ${getFootnoteDepth()}\n" +
                "     ?alias ${getAliases()}\n" +
                "--------Meta---------\n" +
                "     real start position: ${getTextStartPosition()}\n" +
                "     real page width: ${getTextWidth()}\n" +
                "}\n"
    }
}
