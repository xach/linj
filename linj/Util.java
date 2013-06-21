package linj;

import java.math.BigDecimal;

import static java.lang.Math.PI;

public class Util extends Object {

    // methods

    public static String stringCapitalize(String string, int start, int end) {
        StringBuffer newstring = new StringBuffer(string);
        int index = start;
        for (boolean newword = true; index != end; ++index) {
            char _char = string.charAt(index);
            if (! Character.isLetterOrDigit(_char)) {
                newword = true;
            } else if (newword) {
                _char = Character.toUpperCase(_char);
                newword = false;
            } else {
                _char = Character.toLowerCase(_char);
            }
            newstring.setCharAt(index, _char);
        }
        return newstring.toString();
    }

    public static int gcd(int a, int b) {
        if (b == 0) {
            return Math.abs(a);
        } else {
            return gcd(b, a % b);
        }
    }

    public static long gcd(long a, long b) {
        if (b == 0) {
            return Math.abs(a);
        } else {
            return gcd(b, a % b);
        }
    }

    public static Bignum gcd(Bignum a, Bignum b) {
        if (b.compareTo(Bignum.valueOf(0)) == 0) {
            return a.abs();
        } else {
            return gcd(b, a.remainder(b));
        }
    }

    public static int lcd(int a, int b) {
        if ((a == 0) || (b == 0)) {
            return 0;
        } else {
            return Math.abs(a * b) / gcd(a, b);
        }
    }

    public static long lcd(long a, long b) {
        if ((a == 0) || (b == 0)) {
            return 0;
        } else {
            return Math.abs(a * b) / gcd(a, b);
        }
    }

    public static Bignum lcd(Bignum a, Bignum b) {
        if ((a.compareTo(Bignum.valueOf(0)) == 0) || (b.compareTo(Bignum.valueOf(0)) == 0)) {
            return Bignum.valueOf(0);
        } else {
            return a.multiply(b).abs().divide(gcd(a, b));
        }
    }

    public static int isqrt(int n) {
        int n0 = (int)Math.floor((n + 1) / 2);
        int r = 0;
        for (; n0 > r; ++r) {
            n0 -= r;
        }
        return r;
    }

    public static long isqrt(long n) {
        long n0 = (long)Math.floor((n + 1) / 2);
        long r = 0L;
        for (; n0 > r; ++r) {
            n0 -= r;
        }
        return r;
    }

    public static Bignum isqrt(Bignum n) {
        Bignum n0 = n.add(Bignum.valueOf(1)).floor(Bignum.valueOf(2));
        Bignum r = Bignum.valueOf(0);
        for (; n0.compareTo(r) > 0; r = r.add(Bignum.valueOf(1))) {
            n0 = n0.subtract(r);
        }
        return r;
    }

    public static Bignum rational(long e) {
        return rationalize(e);
    }

    public static Bignum rational(float e) {
        return rationalize(e);
    }

    public static Bignum rational(double e) {
        return rationalize(e);
    }

    public static Bignum rational(BigDecimal e) {
        return rationalize(e.longValue());
    }

    public static Bignum rationalize(long e) {
        return Bignum.valueOf(e);
    }

    public static Bignum rationalize(float e) {
        return rationalize(new BigDecimal("" + e).longValue());
    }

    public static Bignum rationalize(double e) {
        return rationalize(new BigDecimal("" + e).longValue());
    }

    public static int shift(int a, int b) {
        if ((0 < b) && (b < 32)) {
            return a << b;
        } else if ((-32 < b) && (b < 0)) {
            return a >> (- b);
        } else if (b == 0) {
            return a;
        } else if (b >= 32) {
            return 0;
        } else {
            if (a > 0) {
                return 0;
            } else {
                return -1;
            }
        }
    }

    public static long shift(long a, int b) {
        if ((0 < b) && (b < 64)) {
            return a << b;
        } else if ((-64 < b) && (b < 0)) {
            return a >> (- b);
        } else if (b == 0) {
            return a;
        } else if (b >= 64) {
            return 0L;
        } else {
            if (a > 0) {
                return 0L;
            } else {
                return -1L;
            }
        }
    }

    public static int mod(int a, int b) {
        if ((a * b) < 0) {
            return (a % b) + b;
        } else {
            return a % b;
        }
    }

    public static long mod(long a, long b) {
        if ((a * b) < 0) {
            return (a % b) + b;
        } else {
            return a % b;
        }
    }

    public static Bignum mod(Bignum a, Bignum b) {
        if (a.multiply(b).compareTo(Bignum.valueOf(0)) < 0) {
            return a.remainder(b).add(b);
        } else {
            return a.remainder(b);
        }
    }

    public static float mod(float a, float b) {
        if ((a * b) < 0) {
            return (a % b) + b;
        } else {
            return a % b;
        }
    }

    public static double mod(double a, double b) {
        if ((a * b) < 0) {
            return (a % b) + b;
        } else {
            return a % b;
        }
    }

    public static int[] truncate(int a, int b) {
        int quo = a / b;
        return new int[] { quo, a - (quo * b) };
    }

    public static long[] truncate(long a, long b) {
        long quo = a / b;
        return new long[] { quo, a - (quo * b) };
    }

    public static String formatPrintIntegerToString(long number, boolean printCommasP, boolean printSignP, int radix,
                                                    int mincol, char padchar, char commachar, int commainterval) {
        String text = Long.toString(Math.abs(number), radix);
        String commaed = (printCommasP) ? formatAddCommas(text, commachar, commainterval) : text;
        String signed = (number < 0) ? "-" + commaed : (printSignP) ? "+" + commaed : commaed;
        return formatWriteFieldToString(signed, mincol, 1, 0, padchar, true);
    }

    public static String formatAddCommas(String string, char commachar, int commainterval) {
        int length = string.length();
        int[] multipleResults = truncate(length - 1, commainterval);
        int commas = multipleResults[0];
        int extra = multipleResults[1];
        StringBuffer newString = new StringBuffer(length + commas);
        int firstComma = extra + 1;
        newString.setLength(length + commas);
        {
            String replacement = string.substring(0, firstComma);
            newString.replace(0, firstComma, replacement);
        }
        int src = firstComma;
        for (int dst = firstComma; src != length; src = src + commainterval, dst = dst + commainterval + 1) {
            newString.setCharAt(dst, commachar);
            String replacement = string.substring(src, src + commainterval);
            newString.replace(dst + 1, Math.min(newString.length(), 1 + dst + replacement.length()), replacement);
        }
        return newString.toString();
    }

    public static String formatWriteFieldToString(String string, int mincol, int colinc, int minpad, char padchar,
                                                  boolean padleft) {
        StringBuffer str = new StringBuffer();
        minpad = Math.max(0, minpad);
        if (! padleft) {
            str.append(string);
        }
        for (int i = 0; i < minpad; ++i) {
            str.append(padchar);
        }
        for (int chars = string.length() + minpad; chars < mincol; chars = chars + colinc) {
            for (int i = 0; i < colinc; ++i) {
                str.append(padchar);
            }
        }
        if (padleft) {
            str.append(string);
        }
        return str.toString();
    }

    public static String formatPrintOldRomanToString(long n) {
        if (! ((0 < n) && (n < 5000))) {
            throw new Error("Number too large to print in old Roman numerals: " +
                            formatPrintIntegerToString(n, true, false, 10, 0, ' ', ',', 3));
        }
        StringBuffer str = new StringBuffer();
        char[] chars = new char[] { 'M', 'D', 'C', 'L', 'X', 'V', 'I' };
        int[] vals = new int[] { 1000, 500, 100, 50, 10, 5, 1 };
        long start = n;
        int limit = chars.length;
        for (int i = 0; i < limit; ++i) {
            char curChar = chars[i];
            int curVal = vals[i];
            long v = start;
            for (; v >= curVal; v = v - curVal) {
                str.append(curChar);
            }
            start = v;
            if (start == 0) {
                break;
            }
        }
        return str.toString();
    }

    public static String formatPrintRomanToString(long n) {
        if (! ((0 < n) && (n < 4000))) {
            throw new Error("Number too large to print in Roman numerals: " +
                            formatPrintIntegerToString(n, true, false, 10, 0, ' ', ',', 3));
        }
        StringBuffer str = new StringBuffer();
        char[] chars = new char[] { 'M', 'D', 'C', 'L', 'X', 'V', 'I' };
        int[] vals = new int[] { 1000, 500, 100, 50, 10, 5, 1 };
        char[] subChars = new char[] { 'C', 'C', 'X', 'X', 'I', 'I' };
        int[] subVals = new int[] { 100, 100, 10, 10, 1, 1, 0 };
        long start = n;
        int limit = chars.length;
        for (int i = 0; i < limit; ++i) {
            char curChar = chars[i];
            int curVal = vals[i];
            char curSubChar = subChars[i];
            int curSubVal = subVals[i];
            long start0 = n;
            long v = start0;
            for (; v >= curVal; v = v - curVal) {
                str.append(curChar);
            }
            if ((curVal - curSubVal) <= v) {
                str.append(curSubChar);
                str.append(curChar);
                start0 = v - (curVal - curSubVal);
            } else {
                start0 = v;
            }
            if (start0 == 0) {
                break;
            }
        }
        return str.toString();
    }

    // key methods

    public static String stringCapitalizeKey(String string, int start, int end, int argsPassed) {
        return stringCapitalize(string,
                                ((argsPassed & 1) == 0) ? 0 : start,
                                ((argsPassed & 2) == 0) ? string.length() : end);
    }

    // constants

    public static final double pi = PI;
}
