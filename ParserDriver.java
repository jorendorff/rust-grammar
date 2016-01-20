// ParserDriver.java - Test whether the parser can handle a file.
//
// To use this:
//    find srcdir -name '*.rs' | CLASSPATH=".:$CLASSPATH" java ParserDriver
// You'll need ANTLR4 in your CLASSPATH too.

import org.antlr.v4.runtime.*;
import java.io.*;
import java.lang.*;
import java.util.BitSet;
import java.util.Scanner;

class CountingErrorListener extends BaseErrorListener {
    public int count = 0;
    public void syntaxError(Recognizer<?,?> recognizer, Object offendingSymbol,
                            int line, int charPositionInLine,
                            String msg, RecognitionException e) {
        count++;
    }
}

public class ParserDriver {
    public static boolean canParse(String filename) throws IOException {
        System.out.println(filename + ": ");

        ANTLRFileStream infile = new ANTLRFileStream(filename);
        RustLexer lexer = new RustLexer(infile);
        CountingErrorListener lexerErrors = new CountingErrorListener();
        lexer.addErrorListener(lexerErrors);
        
        CommonTokenStream tokens = new CommonTokenStream(lexer);
        RustParser parser = new RustParser(tokens);
        try {
            parser.crate();
        } catch (RecognitionException oops) {
            System.out.println(oops);
            return false;
        }
        int n = lexerErrors.count + parser.getNumberOfSyntaxErrors();
        if (n == 0) {
            System.out.println("ok");
            return true;
        } else {
            System.out.println("" + n + " error(s)");
            return false;
        }
    }

    static int main() {
	boolean ok = true;
        try {
            Scanner stdin = new Scanner(System.in);
            while (stdin.hasNextLine()) {
                String filename = stdin.nextLine();
                if (!canParse(filename))
                    ok = false;
            }
        } catch (IOException exc) {
            exc.printStackTrace();
            return 2;
        }
        return ok ? 0 : 1;
    }

    public static void main(String[] args) {
        System.exit(main());
    }
}
