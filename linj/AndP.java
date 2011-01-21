package linj;

public class AndP extends Predicate {

    // methods

    public boolean funcall(Object obj) {
        if (! p1.funcall(obj)) {
            return false;
        } else {
            return p2.funcall(obj);
        }
    }

    // slots

    protected Predicate p1;

    protected Predicate p2;
}