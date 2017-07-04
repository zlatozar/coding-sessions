package PrintersDevil

class Formattor {

    private static final String COMMANDS_FILE_NAME = 'src/main/resources/example-page.txt'

    private final InterpretLine interpretLine = new InterpretLine()
    private final Environment env

    private final File sourceFile
    private final File resultingFile

    private final StringBuilder sbParagraph = new StringBuilder()
    private final StringBuilder sbFootnote = new StringBuilder()

    private final StringBuilder PAGE = new StringBuilder()

    Formattor(File sourceFile, File resultingFile) {
        this.sourceFile = sourceFile
        this.resultingFile = resultingFile

        this.env = interpretLine.getEnvironment()
    }

    void start() {
        sourceFile.eachLine({
            line ->
                writeInPage(interpretLine.process(line.trim()))

        })
    }

    // Helper functions

    private void writeInPage(String line) {

    }

    private String formLine(String line) {

    }

    private String formPrintArea() {
        StringBuilder indent = new StringBuilder()

        // form start position
        (0..env.getTextStartPosition()).each {
            indent.append(Constants.WORDS_SEP)
        }

        return indent.toString()
    }



    /**
     * How line will be placed in page is defined in paragraph mode
     */
    private String formContent(String pagragraph) {

        String resultingParagraph = pagragraph
        switch (env.getParagraphMode()) {

            case Constants.FILL_mode:
                resultingParagraph = Paragraph.filled(pagragraph, getPageWidth())
                break

            case Constants.JUSTIFY_mode:
                resultingParagraph = Paragraph.justify(pagragraph, getPageWidth())
                break

            default:
                resultingParagraph = Paragraph.unfilled(pagragraph, getPageWidth())
        }

        return resultingParagraph
    }
}
