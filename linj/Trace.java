package linj;

public class Trace extends Object {

    // methods

    public static void printLevel() {
        int limit = (starTraceLevelStar * starTraceOffsetStar) % starMaxTraceIndentationStar;
        for (int i = 0; i < limit; ++i) {
            System.err.print(" ");
        }
        System.err.print(starTraceLevelStar);
        System.err.print(": ");
    }

    public static int in() {
        printLevel();
        return ++starTraceLevelStar;
    }

    public static void out() {
        --starTraceLevelStar;
        printLevel();
    }

    // slots

    public static int starTraceLevelStar = 0;

    public static int starTraceOffsetStar = 2;

    public static int starMaxTraceIndentationStar = 80;
}
