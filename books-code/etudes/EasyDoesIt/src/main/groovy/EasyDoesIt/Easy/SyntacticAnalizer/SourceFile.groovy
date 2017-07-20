package EasyDoesIt.Easy.SyntacticAnalizer

import java.nio.charset.StandardCharsets

public class SourceFile {

    private boolean debug = false;

    public static final char EOL = '\n';      // end of line
    public static final char EOT = '\u0000';  // end of transition

    File sourceFile;
    InputStream source;
    int currentLine;

    public SourceFile(String filename) {

        try {
            this.sourceFile = new File(filename);
            this.source = new FileInputStream(sourceFile);

            this.currentLine = 1;

        } catch (IOException _) {
            this.sourceFile = null;
            this.source = null;

            this.currentLine = 0;
        }
    }

    public SourceFile(String snippet, boolean debug) {

        try {
            this.source = new ByteArrayInputStream(snippet.getBytes(StandardCharsets.UTF_8));
            this.currentLine = 1;

            this.debug = debug

        } catch (IOException s) {
            this.sourceFile = null;
            this.source = null;

            this.currentLine = 0;
        }
    }

    /**
     * Read given file that contains Triangle language code
     *
     * @return chars one by one from the source file
     * @see Scanner
     */
    char getSource() {

        try {
            int c = source.read();

            if (c == -1) {
                c = EOT;

            } else if (c == EOL) {
                currentLine++;
            }

            if (debug) {
                println("symbol: ${(char) c}" )
            }

            return (char) c;

        } catch (IOException _) {
            return EOT;
        }
    }

    int getCurrentLineNumber() {
        return currentLine;
    }
}
