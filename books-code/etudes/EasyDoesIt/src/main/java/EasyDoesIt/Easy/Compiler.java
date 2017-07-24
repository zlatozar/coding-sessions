package EasyDoesIt.Easy;

import EasyDoesIt.Easy.AbstractSyntaxTrees.Program;
import EasyDoesIt.Easy.SyntacticAnalizer.Parser;
import EasyDoesIt.Easy.SyntacticAnalizer.Scanner;
import EasyDoesIt.Easy.SyntacticAnalizer.SourceFile;

/**
 * The main driver class for the Easy compiler.
 */
public class Compiler {

    /**
     * The filename for the object program, normally obj.esy
     */
    static String objectName = "obj.esy";

    private static Scanner scanner;
    private static Parser parser;
    private static ErrorReporter reporter;

    /**
     * The AST representing the source program.
     */
    private static Program theAST;

    /**
     * Compile the source program to TAM machine code.
     *
     * @param sourceName the name of the file containing the
     *                   source program.
     * @param objectName the name of the file containing the
     *                   object program.
     * @param showingAST true iff the AST is to be displayed after
     *                   contextual analysis (not currently implemented).
     * @param showingTable true iff the object description details are to
     *                     be displayed during code generation (not currently implemented).
     *
     * @return true iff the source program is free of compile-time errors,
     *         otherwise false.
     */
    static boolean compileProgram(String sourceName, String objectName, boolean showingAST, boolean showingTable) {

        System.out.println("********** Easy Compiler (Java Version 0.1) **********");

        System.out.println("Syntactic Analysis ...");
        SourceFile source = new SourceFile(sourceName);

        if (source == null) {
            System.out.println("Can't access source file " + sourceName);
            System.exit(1);
        }

        scanner = new Scanner(source);
        reporter = new ErrorReporter();
        parser = new Parser(scanner, reporter);

        theAST = parser.parseProgram();                     // 1st pass

        return reporter.numErrors == 0;
    }

    /**
     * Triangle compiler main program.
     *
     * @param args the only command-line argument to the program specifies
     *             the source filename.
     */
    public static void main(String[] args) {
        boolean compiledOK;

        if (args.length != 1) {
            System.out.println("Usage: esy filename");
            System.exit(1);
        }

        String sourceName = args[0];
        compiledOK = compileProgram(sourceName, objectName, false, false);

        System.out.println("Is compilation pass? " + compiledOK);
    }
}

