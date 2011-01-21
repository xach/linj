package linj;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Arrays;

public class Bignum extends Number implements Comparable {

    // constructors

    public Bignum(BigInteger numerator, BigInteger denominator) {
        this.numerator = numerator;
        this.denominator = denominator;
    }

    // accessors

    public BigInteger numerator() {
        return numerator;
    }

    public BigInteger denominator() {
        return denominator;
    }

    // methods

    public String toString() {
        if (denominator.compareTo(BigInteger.valueOf(1)) == 0) {
            StringBuffer buf = new StringBuffer();
            buf.append(numerator);
            return buf.toString();
        } else {
            StringBuffer buf = new StringBuffer();
            buf.append(numerator);
            buf.append("/");
            buf.append(denominator);
            return buf.toString();
        }
    }

    public String toString(int radix) {
        if (denominator.compareTo(BigInteger.valueOf(1)) == 0) {
            StringBuffer buf = new StringBuffer();
            buf.append(numerator.toString(radix));
            return buf.toString();
        } else {
            StringBuffer buf = new StringBuffer();
            buf.append(numerator.toString(radix));
            buf.append("/");
            buf.append(denominator.toString(radix));
            return buf.toString();
        }
    }

    public static Bignum fraction(BigInteger num, BigInteger den) {
        if (den.equals(ONE.numerator())) {
            if (num.equals(ZERO.numerator())) {
                return ZERO;
            } else if (num.equals(ONE.numerator())) {
                return ONE;
            } else {
                return new Bignum(num, BigInteger.valueOf(1));
            }
        } else {
            int signum = den.signum();
            if (signum == 0) {
                throw new Error("Division by zero");
            }
            if (signum < 0) {
                num = num.negate();
                den = den.negate();
            }
            BigInteger gcd = num.gcd(den);
            return new Bignum(num.divide(gcd), den.divide(gcd));
        }
    }

    public static Bignum valueOf(BigInteger num, BigInteger den) {
        return fraction(num, den);
    }

    public static Bignum valueOf(BigInteger num) {
        return fraction(num, BigInteger.valueOf(1));
    }

    public static Bignum valueOf(long num) {
        return fraction(BigInteger.valueOf(num), BigInteger.valueOf(1));
    }

    public static Bignum valueOf(long num, long den) {
        return fraction(BigInteger.valueOf(num), BigInteger.valueOf(den));
    }

    public static Bignum valueOf(String num) {
        int idx = num.indexOf('/');
        if (idx < 0) {
            return valueOf(new BigInteger(num));
        } else {
            return fraction(new BigInteger(num.substring(0, idx)), new BigInteger(num.substring(idx + 1)));
        }
    }

    public Bignum add(Bignum other) {
        return fraction(numerator.multiply(other.denominator).add(other.numerator.multiply(denominator)),
                        denominator.multiply(other.denominator));
    }

    public Bignum subtract(Bignum other) {
        return fraction(numerator.multiply(other.denominator).subtract(other.numerator.multiply(denominator)),
                        denominator.multiply(other.denominator));
    }

    public Bignum multiply(Bignum other) {
        return fraction(numerator.multiply(other.numerator), denominator.multiply(other.denominator));
    }

    public Bignum divide(Bignum other) {
        return fraction(numerator.multiply(other.denominator), denominator.multiply(other.numerator));
    }

    public Bignum negate() {
        return fraction(numerator.negate(), denominator);
    }

    public Bignum pow(long other) {
        Bignum base = ONE;
        for (int i = 0; i < other; ++i) {
            base = multiply(base);
        }
        return base;
    }

    public Bignum remainder(Bignum other) {
        Bignum quotient = divide(other).floor();
        return subtract(quotient.multiply(other));
    }

    public Bignum floor() {
        return valueOf(numerator().divide(denominator()));
    }

    public Bignum floor(Bignum other) {
        return divide(other).floor();
    }

    public Bignum round() {
        return add(ONE_HALF).floor();
    }

    public boolean equals(Bignum other) {
        if (numerator().compareTo(other.numerator()) != 0) {
            return false;
        } else {
            return denominator().compareTo(other.denominator()) == 0;
        }
    }

    public boolean equals(Object other) {
        if (this == other) {
            return true;
        } else if (other == null) {
            return false;
        } else if (getClass() != other.getClass()) {
            return false;
        } else {
            Bignum otherBignum = (Bignum)other;
            if (numerator().compareTo(otherBignum.numerator()) != 0) {
                return false;
            } else {
                return denominator().compareTo(otherBignum.denominator()) == 0;
            }
        }
    }

    public int hashCode() {
        return (37 * numerator().hashCode()) + denominator().hashCode();
    }

    public int compareTo(Bignum other) {
        BigInteger num1 = numerator().multiply(other.denominator());
        BigInteger num2 = other.numerator().multiply(denominator());
        return num1.compareTo(num2);
    }

    public int compareTo(Object other) {
        return compareTo((Bignum)other);
    }

    public Bignum and(Bignum other) {
        assertIsInteger();
        other.assertIsInteger();
        return valueOf(numerator().and(other.numerator()));
    }

    public Bignum or(Bignum other) {
        assertIsInteger();
        other.assertIsInteger();
        return valueOf(numerator().or(other.numerator()));
    }

    public Bignum xor(Bignum other) {
        assertIsInteger();
        other.assertIsInteger();
        return valueOf(numerator().xor(other.numerator()));
    }

    public BigInteger shiftLeft(int n) {
        assertIsInteger();
        return numerator().shiftLeft(n);
    }

    public BigInteger shiftRight(int n) {
        assertIsInteger();
        return numerator().shiftRight(n);
    }

    public boolean testBit(int n) {
        assertIsInteger();
        return numerator().testBit(n);
    }

    public Bignum not() {
        assertIsInteger();
        return valueOf(numerator().not());
    }

    public Bignum abs() {
        if (numerator.compareTo(BigInteger.valueOf(0)) < 0) {
            return negate();
        } else {
            return this;
        }
    }

    public int signum() {
        if (numerator.compareTo(BigInteger.valueOf(0)) < 0) {
            return -1;
        } else if (numerator.compareTo(BigInteger.valueOf(0)) == 0) {
            return 0;
        } else {
            return 1;
        }
    }

    public Bignum min(Bignum other) {
        if (compareTo(other) <= 0) {
            return this;
        } else {
            return other;
        }
    }

    public Bignum max(Bignum other) {
        if (compareTo(other) <= 0) {
            return other;
        } else {
            return this;
        }
    }

    public void assertIsInteger() {
        if (denominator.compareTo(BigInteger.valueOf(1)) != 0) {
            throw new Error("The bignum " + this + " is a rational and can't be converted to integer");
        }
    }

    public byte byteValue() {
        assertIsInteger();
        return numerator().byteValue();
    }

    public short shortValue() {
        assertIsInteger();
        return numerator().shortValue();
    }

    public int intValue() {
        assertIsInteger();
        return numerator().intValue();
    }

    public long longValue() {
        assertIsInteger();
        return numerator().longValue();
    }

    public BigDecimal toBigDecimal() {
        return new BigDecimal(numerator(), 100).divide(new BigDecimal(denominator(), 100), BigDecimal.ROUND_HALF_UP);
    }

    public BigInteger toBigInteger() {
        assertIsInteger();
        return numerator();
    }

    public float floatValue() {
        return toBigDecimal().floatValue();
    }

    public double doubleValue() {
        return toBigDecimal().doubleValue();
    }

    public boolean integerp() {
        return denominator.compareTo(BigInteger.valueOf(1)) == 0;
    }

    public boolean ratiop() {
        return denominator.compareTo(BigInteger.valueOf(1)) != 0;
    }

    public static void main(String[] outsideArgs) {
        Bignum x = fraction(BigInteger.valueOf(1), BigInteger.valueOf(3));
        Bignum y = fraction(BigInteger.valueOf(1), BigInteger.valueOf(4));
        Bignum z = valueOf("2/6");
        System.out.println(z);
        System.out.print(x);
        System.out.print(" + ");
        System.out.print(x);
        System.out.print(" = ");
        System.out.println(x.add(x));
        System.out.print(x);
        System.out.print(" - ");
        System.out.print(x);
        System.out.print(" = ");
        System.out.println(x.subtract(x));
        System.out.print(x);
        System.out.print(" * ");
        System.out.print(x);
        System.out.print(" = ");
        System.out.println(x.multiply(x));
        System.out.print(x);
        System.out.print(" / ");
        System.out.print(x);
        System.out.print(" = ");
        System.out.println(x.divide(x));
        System.out.print(x);
        System.out.print(" rem ");
        System.out.print(y);
        System.out.print(" = ");
        System.out.println(x.remainder(y));
        System.out.print(x);
        System.out.print(" = ");
        System.out.println(x.toBigDecimal());
        System.out.print(x);
        System.out.print(" compareTo ");
        System.out.print(y);
        System.out.print(" = ");
        System.out.println(x.compareTo(y));
        Bignum[] nums =
            new Bignum[] {
                fraction(BigInteger.valueOf(1), BigInteger.valueOf(3)),
                fraction(BigInteger.valueOf(1), BigInteger.valueOf(4)),
                fraction(BigInteger.valueOf(1), BigInteger.valueOf(5)),
                fraction(BigInteger.valueOf(1), BigInteger.valueOf(1)),
                fraction(BigInteger.valueOf(1), BigInteger.valueOf(2)) };
        {
            int limit = nums.length;
            for (int i = 0; i < limit; ++i) {
                Bignum e = nums[i];
                System.out.print("" + '\n' + e + " ");
            }
        }
        Arrays.sort(nums);
        int limit = nums.length;
        for (int i = 0; i < limit; ++i) {
            Bignum e = nums[i];
            System.out.print("" + '\n' + e + " ");
        }
    }

    // constants

    public static final Bignum ZERO = new Bignum(BigInteger.valueOf(0), BigInteger.valueOf(1));

    public static final Bignum ONE = new Bignum(BigInteger.valueOf(1), BigInteger.valueOf(1));

    public static final Bignum ONE_HALF = new Bignum(BigInteger.valueOf(1), BigInteger.valueOf(2));

    // slots

    protected BigInteger numerator;

    protected BigInteger denominator;
}