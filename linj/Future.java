package linj;

public class Future extends Object {

    // constructors

    public Future(final Function0 f) {
        new Thread() {
            public void run() {
                computeValue(f);
            }}.
         start();
    }

    // methods

    public synchronized Object getValue() {
        try {
            while (! available) {
                wait();
            }
        } catch (Throwable e) {
        }
        notify();
        return value;
    }

    public synchronized void computeValue(Function0 f) {
        value = f.funcall();
        available = true;
        notify();
    }

    // slots

    protected Object value;

    protected boolean available = false;
}
