package EasyDoesIt.Easy.SyntacticAnalizer;

/**
 * The interface between the scanner and the parser is a stream of tokens.
 * A token is an atomic symbol of the source program. The interface between
 * the scanner and the parser is a stream of tokens.
 */
final class Token {

    private final static int firstReservedWord = Token.ARRAY;
    private final static int lastReservedWord = Token.WHILE;

    private static String[] spellingTable =
            {
                    "<int>",
                    "<char>",
                    "<identifier>",
                    "<operator>",

                    "ARRAY",
                    "BEGIN",
                    "BY",
                    "CALL",
                    "CASE",
                    "DECLARE",
                    "ELSE",
                    "END",
                    "EXIT",
                    "FI",
                    "FIELD",
                    "FOR",
                    "FUNCTION",
                    "IF",
                    "IS",
                    "OF",
                    "OTHERWISE",
                    "PROCEDURE",
                    "PROGRAM",
                    "REPEAT",
                    "REPENT",
                    "RETURN",
                    "SELECT",
                    "SET",
                    "STRUCTURE",
                    "THEN",
                    "TO",
                    "TYPE",
                    "WHILE",

                    ".",
                    ":",
                    ";",
                    ",",
                    ":=",

                    "(",
                    ")",
                    "[",
                    "]",

                    "",       // EOT
                    "<error>"
            };

    protected int kind;
    protected final String spelling;
    protected final SourcePosition position;

    // Each token is completely described by its kind and spelling
    public Token(int kind, String spelling, SourcePosition position) {

        // The lexical grammar did not distinguish between identifiers and keywords.
        // Nevertheless, the scanner must properly classify these tokens.
        if (kind == Token.IDENTIFIER) {

            int currentKind = firstReservedWord;
            boolean searching = true;

            while (searching) {
                int comparison = spellingTable[currentKind].compareTo(spelling);

                if (comparison == 0) {
                    this.kind = currentKind;
                    searching = false;

                } else if (comparison > 0 || currentKind == lastReservedWord) {
                    this.kind = Token.IDENTIFIER;
                    searching = false;

                } else {
                    currentKind++;
                }
            }

        } else {
            this.kind = kind;

        }

        this.spelling = spelling;
        this.position = position;
    }

    public static String spell(int kind) {
        return spellingTable[kind];
    }

//_____________________________________________________________________________
//                               Constants denoting different kinds of <Token>

    // literals, identifiers, operators...

    public static final int INTLITERAL    = 0;
    public static final int STRINGLITERAL = 1;
    public static final int IDENTIFIER    = 2;
    public static final int OPERATOR      = 3;

    // reserved words - must be in alphabetical order...

    public static final int ARRAY =      4;   // first reserved word
    public static final int BEGIN =      5;
    public static final int BY =         6;
    public static final int CALL =       7;
    public static final int CASE =       8;
    public static final int DECLARE =    9;
    public static final int ELSE =      10;
    public static final int END =       11;
    public static final int EXIT =      12;
    public static final int FI =        13;
    public static final int FIELD =     14;
    public static final int FOR =       15;
    public static final int FUNCTION =  16;
    public static final int IF =        17;
    public static final int IS =        18;
    public static final int OF =        19;
    public static final int OTHERWISE = 20;
    public static final int PROCEDURE = 21;
    public static final int PROGRAM =   22;
    public static final int REPEAT =    23;
    public static final int REPENT =    24;
    public static final int RETURN =    25;
    public static final int SELECT =    26;
    public static final int SET =       27;
    public static final int STRUCTURE = 28;
    public static final int THEN =      29;
    public static final int TO =        30;
    public static final int TYPE =      31;
    public static final int WHILE =     32;  // last reserved word

    // punctuation...

    public static final int DOT       = 33;
    public static final int COLON     = 34;
    public static final int SEMICOLON = 35;
    public static final int COMMA     = 36;
    public static final int BECOMES   = 37;  // :=

    // brackets...

    public static final int LPAREN   = 38;
    public static final int RPAREN   = 39;
    public static final int LBRACKET = 40;
    public static final int RBRACKET = 41;

    // special tokens...

    // Note that EOT represents the end of the source text
    public static final int EOT   = 42;
    public static final int ERROR = 43;

    @Override
    public String toString() {
        return "Kind=" + kind + ", spelling=" + spelling + ", position=" + position;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Token token = (Token) o;
        return kind == token.kind && spelling.equals(token.spelling);
    }

    @Override
    public int hashCode() {
        int result = kind;
        result = 31 * result + spelling.hashCode();
        return result;
    }
}
