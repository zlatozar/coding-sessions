/*
 * @(#)SourceFile.java                        2.1 2003/10/07
 *
 * Copyright (C) 1999, 2003 D.A. Watt and D.F. Brown
 * Dept. of Computing Science, University of Glasgow, Glasgow G12 8QQ Scotland
 * and School of Computer and Math Sciences, The Robert Gordon University,
 * St. Andrew Street, Aberdeen AB25 1HG, Scotland.
 * All rights reserved.
 *
 * This software is provided free for educational use only. It may
 * not be used for commercial purposes without the prior written permission
 * of the authors.
 */

package Triangle.SyntacticAnalyzer;

import java.io.*;
import java.nio.charset.StandardCharsets;

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

        } catch (IOException s) {
            this.sourceFile = null;
            this.source = null;

            this.currentLine = 0;
        }
    }

    public SourceFile(String snippet, boolean debug) {

        try {
            this.source = new ByteArrayInputStream(snippet.getBytes(StandardCharsets.UTF_8));
            this.currentLine = 1;

            this.debug = debug;

        } catch (Exception s) {
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
                System.out.println("source: " + (char) c);
            }

            return (char) c;

        } catch (IOException s) {
            return EOT;
        }
    }

    int getCurrentLineNumber() {
        return currentLine;
    }
}
