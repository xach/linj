package linj;

public class Predicate2 extends Object {

    // methods

    public boolean funcall(Object arg1, Object arg2) {
        return true;
    }

    // constants

    public static final Predicate2 EQ_FUNCTION =
        new Predicate2() {
            public boolean funcall(Object obj1, Object obj2) {
                return obj1 == obj2;
            }};

    public static final Predicate2 EQL_FUNCTION =
        new Predicate2() {
            public boolean funcall(Object obj1, Object obj2) {
                return (obj1 == obj2) || ((obj1 instanceof Number) && (obj2 instanceof Number) && obj1.equals(obj2));
            }};

    public static final Predicate2 EQUALS_FUNCTION =
        new Predicate2() {
            public boolean funcall(Object obj1, Object obj2) {
                return obj1.equals(obj2);
            }};
}
