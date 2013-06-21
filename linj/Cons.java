package linj;

public class Cons extends Object {

    // constructors

    public Cons(Object car, Object cdr) {
        this.car = car;
        this.cdr = cdr;
    }

    // accessors

    public Object car() {
        return car;
    }

    public Object setfCar(Object car) {
        return this.car = car;
    }

    public Object cdr() {
        return cdr;
    }

    public Object setfCdr(Object cdr) {
        return this.cdr = cdr;
    }

    // methods

    public static Cons list() {
        return Cons.EMPTY_LIST;
    }

    public static Cons list(Object a0) {
        return new Cons(a0, Cons.EMPTY_LIST);
    }

    public static Cons list(Object a0, Object a1) {
        return new Cons(a0, new Cons(a1, Cons.EMPTY_LIST));
    }

    public static Cons list(Object a0, Object a1, Object a2) {
        return new Cons(a0, new Cons(a1, new Cons(a2, Cons.EMPTY_LIST)));
    }

    public static Cons list(Object a0, Object a1, Object a2, Object a3) {
        return new Cons(a0, new Cons(a1, new Cons(a2, new Cons(a3, Cons.EMPTY_LIST))));
    }

    public static Cons list(Object a0, Object a1, Object a2, Object a3, Object a4) {
        return new Cons(a0, new Cons(a1, new Cons(a2, new Cons(a3, new Cons(a4, Cons.EMPTY_LIST)))));
    }

    public boolean endp() {
        return this == EMPTY_LIST;
    }

    public Object first() {
        if (endp()) {
            throw new Error("Attempted method first on the empty list: " + this);
        } else {
            return car();
        }
    }

    public Object setfFirst(Object first) {
        if (endp()) {
            throw new Error("Attempted method rest on the empty list: " + this);
        } else {
            return setfCar(first);
        }
    }

    public Cons rest() {
        if (endp()) {
            throw new Error("Attempted method rest on the empty list: " + this);
        } else {
            return (Cons)cdr();
        }
    }

    public Object setfRest(Cons rest) {
        if (endp()) {
            throw new Error("Attempted method (setf rest) on the empty list: " + this);
        } else {
            return setfCdr(rest);
        }
    }

    public Object caar() {
        return ((Cons)car()).car();
    }

    public Object cadr() {
        return ((Cons)cdr()).car();
    }

    public Object cdar() {
        return ((Cons)car()).cdr();
    }

    public Object cddr() {
        return ((Cons)cdr()).cdr();
    }

    public int length() {
        int count = 0;
        Cons listE = this;
        for (; ! listE.endp(); listE = listE.rest()) {
            Object e = listE.first();
            ++count;
        }
        return count;
    }

    public int position(Object elem, Predicate2 test, Function key) {
        Cons listE = this;
        for (int i = 0; ! listE.endp(); ++i) {
            Object e = listE.first();
            if (test.funcall(elem, key.funcall(e))) {
                return i;
            }
            listE = listE.rest();
        }
        return -1;
    }

    public Object nth(int n) {
        int i = 1;
        Cons l0 = this;
        for (; (i < n) && (! l0.endp()); l0 = l0.rest()) {
            ++i;
        }
        return l0.first();
    }

    public Object setfNth(Object value, int n) {
        return nthcdr(n - 1).setfFirst(value);
    }

    public Cons iota(int n) {
        Cons collect = list();
        Cons tail = list();
        for (int i = 0; i <= n; ++i) {
            if (collect.endp()) {
                collect = list(Bignum.valueOf(i));
                tail = (Cons)collect.last(1);
            } else {
                tail.nconc(list(Bignum.valueOf(i)));
                tail = (Cons)tail.last(1);
            }
        }
        return collect;
    }

    public Object find(Object elem, Predicate2 test, Function key) {
        Cons listE = this;
        for (; ! listE.endp(); listE = listE.rest()) {
            Object e = listE.first();
            if (test.funcall(elem, key.funcall(e))) {
                return e;
            }
        }
        return null;
    }

    public Object findIf(Predicate pred) {
        Cons listE = this;
        for (; ! listE.endp(); listE = listE.rest()) {
            Object e = listE.first();
            if (pred.funcall(e)) {
                return e;
            }
        }
        return null;
    }

    public int positionIf(Predicate pred) {
        Cons listE = this;
        for (int i = 0; ! listE.endp(); ++i) {
            Object e = listE.first();
            if (pred.funcall(e)) {
                return i;
            }
            listE = listE.rest();
        }
        return -1;
    }

    public Cons member(Object elem, Predicate2 test, Function key) {
        Cons l = this;
        for (; ! l.endp(); l = l.rest()) {
            if (test.funcall(elem, key.funcall(l.first()))) {
                break;
            }
        }
        return l;
    }

    public Cons memberIf(Predicate test, Function key) {
        Cons l = this;
        for (; ! l.endp(); l = l.rest()) {
            if (test.funcall(key.funcall(l.first()))) {
                break;
            }
        }
        return l;
    }

    public Cons append(Cons other) {
        return copyList().nconc(other);
    }

    public Cons nconc(Cons other) {
        if (endp()) {
            return other;
        } else {
            ((Cons)last(1)).setfRest(other);
            return this;
        }
    }

    public Cons mapcar(Function f) {
        Cons collect = list();
        Cons tail = list();
        Cons listE = this;
        for (; ! listE.endp(); listE = listE.rest()) {
            Object e = listE.first();
            if (collect.endp()) {
                collect = list(f.funcall(e));
                tail = (Cons)collect.last(1);
            } else {
                tail.nconc(list(f.funcall(e)));
                tail = (Cons)tail.last(1);
            }
        }
        return collect;
    }

    public Cons mappend(Function f) {
        Cons appending = list();
        Cons tail = list();
        Cons listE = this;
        for (; ! listE.endp(); listE = listE.rest()) {
            Object e = listE.first();
            if (appending.endp()) {
                appending = ((Cons)f.funcall(e)).copyList();
                tail = (Cons)appending.last(1);
            } else {
                tail.nconc(((Cons)f.funcall(e)).copyList());
                tail = (Cons)tail.last(1);
            }
        }
        return appending;
    }

    public Cons mapcan(Function f) {
        Cons nconcing = list();
        Cons tail = list();
        Cons listE = this;
        for (; ! listE.endp(); listE = listE.rest()) {
            Object e = listE.first();
            if (nconcing.endp()) {
                nconcing = (Cons)f.funcall(e);
                tail = (Cons)nconcing.last(1);
            } else {
                tail.nconc((Cons)f.funcall(e));
                tail = (Cons)tail.last(1);
            }
        }
        return nconcing;
    }

    public void mapc(Procedure f) {
        Cons listE = this;
        for (; ! listE.endp(); listE = listE.rest()) {
            Object e = listE.first();
            f.funcall(e);
        }
    }

    public void mapc(Function f) {
        Cons listE = this;
        for (; ! listE.endp(); listE = listE.rest()) {
            Object e = listE.first();
            f.funcall(e);
        }
    }

    public Cons mapcar(Function2 f, Cons other) {
        Cons collect = list();
        Cons tail = list();
        Cons listE1 = this;
        Cons listE2 = other;
        for (; (! listE1.endp()) && (! listE2.endp()); listE1 = listE1.rest(), listE2 = listE2.rest()) {
            Object e1 = listE1.first();
            Object e2 = listE2.first();
            if (collect.endp()) {
                collect = list(f.funcall(e1, e2));
                tail = (Cons)collect.last(1);
            } else {
                tail.nconc(list(f.funcall(e1, e2)));
                tail = (Cons)tail.last(1);
            }
        }
        return collect;
    }

    public void mapc(Procedure2 f, Cons other) {
        Cons listE1 = this;
        Cons listE2 = other;
        for (; (! listE1.endp()) && (! listE2.endp()); listE1 = listE1.rest(), listE2 = listE2.rest()) {
            Object e1 = listE1.first();
            Object e2 = listE2.first();
            f.funcall(e1, e2);
        }
    }

    public Cons mappend(Function2 f, Cons other) {
        Cons appending = list();
        Cons tail = list();
        Cons listE1 = this;
        Cons listE2 = other;
        for (; (! listE1.endp()) && (! listE2.endp()); listE1 = listE1.rest(), listE2 = listE2.rest()) {
            Object e1 = listE1.first();
            Object e2 = listE2.first();
            if (appending.endp()) {
                appending = ((Cons)f.funcall(e1, e2)).copyList();
                tail = (Cons)appending.last(1);
            } else {
                tail.nconc(((Cons)f.funcall(e1, e2)).copyList());
                tail = (Cons)tail.last(1);
            }
        }
        return appending;
    }

    public Cons removeDuplicates(Predicate2 test) {
        if (endp()) {
            return this;
        } else if (! rest().memberKey(first(), test, null, 1).endp()) {
            return rest().removeDuplicates(test);
        } else {
            return new Cons(first(), rest().removeDuplicates(test));
        }
    }

    public Cons remove(Object elem, Predicate2 test, Function key, int count) {
        if (endp() || (count == 0)) {
            return this;
        } else if (test.funcall(elem, (key == null) ? first() : key.funcall(first()))) {
            return rest().remove(elem, test, key, count - 1);
        } else {
            return new Cons(first(), rest().remove(elem, test, key, count));
        }
    }

    public Cons removeIf(Predicate test) {
        Cons collect = list();
        Cons tail = list();
        Cons listE = this;
        for (; ! listE.endp(); listE = listE.rest()) {
            Object e = listE.first();
            if (! test.funcall(e)) {
                if (collect.endp()) {
                    collect = list(e);
                    tail = (Cons)collect.last(1);
                } else {
                    tail.nconc(list(e));
                    tail = (Cons)tail.last(1);
                }
            }
        }
        return collect;
    }

    public Cons removeIfNot(Predicate test) {
        Cons collect = list();
        Cons tail = list();
        Cons listE = this;
        for (; ! listE.endp(); listE = listE.rest()) {
            Object e = listE.first();
            if (test.funcall(e)) {
                if (collect.endp()) {
                    collect = list(e);
                    tail = (Cons)collect.last(1);
                } else {
                    tail.nconc(list(e));
                    tail = (Cons)tail.last(1);
                }
            }
        }
        return collect;
    }

    public Cons delete(Object elem, Predicate2 test, Function key, int count) {
        if (endp() || (count == 0)) {
            return this;
        } else if (test.funcall(elem, (key == null) ? first() : key.funcall(first()))) {
            return rest().delete(elem, test, key, count - 1);
        } else {
            setfCdr(rest().delete(elem, test, key, count));
            return this;
        }
    }

    public Cons adjoin(Object elem, Predicate2 test) {
        if (! memberKey(elem, test, null, 1).endp()) {
            return this;
        } else {
            return new Cons(elem, this);
        }
    }

    public Cons reverse(Cons result) {
        Cons listE = this;
        for (; ! listE.endp(); listE = listE.rest()) {
            Object e = listE.first();
            result = new Cons(e, result);
        }
        return result;
    }

    public Object last(int n) {
        Object checkedList = (Object)this;
        Object returnedList = (Object)this;
        for (int index = 0;
             ! ((checkedList == Cons.EMPTY_LIST) || (! (checkedList instanceof Cons)));
             checkedList = ((Cons)checkedList).cdr(),
             ++index) {
            if (index >= n) {
                returnedList = ((Cons)returnedList).cdr();
            }
        }
        return returnedList;
    }

    public boolean lengthNP(int n) {
        if ((n == 0) && endp()) {
            return true;
        } else if (n == 0) {
            return false;
        } else if (endp()) {
            return false;
        } else {
            return rest().lengthNP(n - 1);
        }
    }

    public Cons nthcdr(int n) {
        Cons result = this;
        for (int i = n; i > 0; --i, result = (Cons)result.cdr()) {
        }
        return result;
    }

    public Cons butlast(int n) {
        if (lengthNP(n)) {
            return Cons.EMPTY_LIST;
        } else {
            return new Cons(first(), rest().butlast(n));
        }
    }

    public Cons subseq(int start, int end) {
        return nthcdr(start).butlast(length() - end);
    }

    public Object getf(Object indicator, Object _default) {
        if (endp()) {
            return _default;
        } else if (indicator == first()) {
            return second();
        } else {
            return rest().rest().getf(indicator, _default);
        }
    }

    public Cons putPlist(Object indicator, Object value) {
        for (Cons l = this; ! l.endp(); l = l.rest().rest()) {
            if (l.first() == indicator) {
                l.rest().setfCar(value);
                return this;
            }
        }
        return new Cons(indicator, new Cons(value, this));
    }

    public Cons remPlist(Object indicator) {
        Cons l2 = list();
        for (Cons l1 = this; ! l1.endp(); l2 = l1, l1 = l1.rest().rest()) {
            if (l1.car() == indicator) {
                if (l2.endp()) {
                    return rest().rest();
                } else {
                    l2.rest().setfCdr(l1.rest().cdr());
                    return this;
                }
            }
        }
        return this;
    }

    public String toString() {
        StringBuffer buf = new StringBuffer("(");
        Object l = (Object)this;
        for (boolean firstTimeP = true;
             (l instanceof Cons) && (l != Cons.EMPTY_LIST);
             l = ((Cons)l).cdr(),
             firstTimeP = false) {
            if (! firstTimeP) {
                buf.append(" ");
            }
            buf.append(elemToString(((Cons)l).car()));
        }
        if (l == Cons.EMPTY_LIST) {
            buf.append(")");
        } else {
            buf.append(" . ");
            buf.append(elemToString(l));
            buf.append(")");
        }
        return buf.toString();
    }

    public String elemToString(Object elem) {
        if (elem instanceof String) {
            StringBuffer buf = new StringBuffer();
            buf.append("\"");
            buf.append(elem);
            buf.append("\"");
            return buf.toString();
        } else if (elem instanceof Boolean) {
            if (((Boolean)elem).booleanValue()) {
                return "t";
            } else {
                return "nil";
            }
        } else if (elem != null) {
            return elem.toString();
        } else {
            return "null";
        }
    }

    public Object second() {
        return rest().first();
    }

    public Object third() {
        return rest().rest().first();
    }

    public Object fourth() {
        return rest().rest().rest().first();
    }

    public Object fifth() {
        return rest().rest().rest().rest().first();
    }

    public Cons assoc(Object item, Predicate2 test) {
        Object pair = find(item, test, Cons.CAR_FUNCTION);
        if (pair == null) {
            return list();
        } else {
            return (Cons)pair;
        }
    }

    public Object assocGet(Object item, Cons _default, Predicate2 test) {
        Cons result = assoc(item, test);
        if (result == null) {
            return _default;
        } else {
            return result.cdr();
        }
    }

    public boolean every(Predicate pred) {
        if (endp()) {
            return true;
        } else if (! pred.funcall(first())) {
            return false;
        } else {
            return rest().every(pred);
        }
    }

    public boolean every(Predicate2 pred, Cons other) {
        if (endp()) {
            return true;
        } else if (! pred.funcall(first(), other.first())) {
            return false;
        } else {
            return rest().every(pred, other.rest());
        }
    }

    public boolean some(Predicate pred) {
        if (endp()) {
            return false;
        } else if (pred.funcall(first())) {
            return true;
        } else {
            return rest().some(pred);
        }
    }

    public Cons pairlist(Cons other, Cons aList) {
        if (endp()) {
            return aList;
        } else {
            return new Cons(list(first(), other.first()), rest().pairlist(other.rest(), aList));
        }
    }

    public Cons pairlis(Cons other, Cons aList) {
        if (endp()) {
            return aList;
        } else {
            return new Cons(new Cons(first(), other.first()), rest().pairlis(other.rest(), aList));
        }
    }

    public Cons copyList() {
        if (endp()) {
            return this;
        } else {
            return new Cons(first(),
                            ((cdr() instanceof Cons) && (cdr() != Cons.EMPTY_LIST)) ? rest().copyList() : cdr());
        }
    }

    public int listLength() {
        int n = 0;
        Cons fast = this;
        for (Cons slow = this; ! fast.endp(); n = n + 2, fast = fast.rest().rest(), slow = slow.rest()) {
            if (((Cons)fast.cdr()).endp()) {
                return n + 1;
            }
            if ((fast == slow) && (n > 0)) {
                return -1;
            }
        }
        return n;
    }

    public boolean equal(Object other) {
        if (other instanceof Cons) {
            Cons other0 = (Cons)other;
            if (endp()) {
                return other0.endp();
            } else if (! endp()) {
                if (other0.endp()) {
                    return false;
                } else {
                    Object thisCar = car();
                    Object thisCdr = cdr();
                    Object otherCar = other0.car();
                    Object otherCdr = other0.cdr();
                    if (! ((thisCar instanceof Cons) ?
                           ((Cons)thisCar).equal(otherCar) :
                           ((thisCar == otherCar) ||
                            ((thisCar instanceof Number) &&
                             (otherCar instanceof Number) &&
                             thisCar.equals(otherCar))))) {
                        return false;
                    } else if (thisCdr instanceof Cons) {
                        return ((Cons)thisCdr).equal(otherCdr);
                    } else {
                        return (thisCdr == otherCdr) ||
                               ((thisCdr instanceof Number) &&
                                (otherCdr instanceof Number) &&
                                thisCdr.equals(otherCdr));
                    }
                }
            } else {
                return false;
            }
        } else {
            return false;
        }
    }

    public Object reduce(Function2 comb, Object initialValue) {
        if (endp()) {
            return initialValue;
        } else {
            return comb.funcall(first(), rest().reduce(comb, initialValue));
        }
    }

    // key methods

    public int positionKey(Object elem, Predicate2 test, Function key, int argsPassed) {
        return position(elem,
                        ((argsPassed & 1) == 0) ? Predicate2.EQL_FUNCTION : test,
                        ((argsPassed & 2) == 0) ? Cons.IDENTITY_FUNCTION : key);
    }

    public Object findKey(Object elem, Predicate2 test, Function key, int argsPassed) {
        return find(elem,
                    ((argsPassed & 1) == 0) ? Predicate2.EQL_FUNCTION : test,
                    ((argsPassed & 2) == 0) ? Cons.IDENTITY_FUNCTION : key);
    }

    public Cons memberKey(Object elem, Predicate2 test, Function key, int argsPassed) {
        return member(elem,
                      ((argsPassed & 1) == 0) ? Predicate2.EQ_FUNCTION : test,
                      ((argsPassed & 2) == 0) ? Cons.IDENTITY_FUNCTION : key);
    }

    public Cons memberIfKey(Predicate test, Function key, int argsPassed) {
        return memberIf(test, ((argsPassed & 1) == 0) ? Cons.IDENTITY_FUNCTION : key);
    }

    public Cons removeDuplicatesKey(Predicate2 test, int argsPassed) {
        return removeDuplicates(((argsPassed & 1) == 0) ? Predicate2.EQ_FUNCTION : test);
    }

    public Cons removeKey(Object elem, Predicate2 test, Function key, int count, int argsPassed) {
        return remove(elem,
                      ((argsPassed & 1) == 0) ? Predicate2.EQ_FUNCTION : test,
                      ((argsPassed & 2) == 0) ? null : key,
                      ((argsPassed & 4) == 0) ? -1 : count);
    }

    public Cons deleteKey(Object elem, Predicate2 test, Function key, int count, int argsPassed) {
        return delete(elem,
                      ((argsPassed & 1) == 0) ? Predicate2.EQ_FUNCTION : test,
                      ((argsPassed & 2) == 0) ? null : key,
                      ((argsPassed & 4) == 0) ? -1 : count);
    }

    public Cons adjoinKey(Object elem, Predicate2 test, int argsPassed) {
        return adjoin(elem, ((argsPassed & 1) == 0) ? Predicate2.EQ_FUNCTION : test);
    }

    public Cons reverseKey(Cons result, int argsPassed) {
        return reverse(((argsPassed & 1) == 0) ? Cons.EMPTY_LIST : result);
    }

    public Cons subseqKey(int start, int end, int argsPassed) {
        return subseq(((argsPassed & 1) == 0) ? 0 : start, ((argsPassed & 2) == 0) ? length() : end);
    }

    public Cons assocKey(Object item, Predicate2 test, int argsPassed) {
        return assoc(item, ((argsPassed & 1) == 0) ? Predicate2.EQ_FUNCTION : test);
    }

    public Object assocGetKey(Object item, Cons _default, Predicate2 test, int argsPassed) {
        return assocGet(item,
                        ((argsPassed & 1) == 0) ? Cons.EMPTY_LIST : _default,
                        ((argsPassed & 2) == 0) ? Predicate2.EQ_FUNCTION : test);
    }

    public Cons pairlistKey(Cons other, Cons aList, int argsPassed) {
        return pairlist(other, ((argsPassed & 1) == 0) ? list() : aList);
    }

    public Cons pairlisKey(Cons other, Cons aList, int argsPassed) {
        return pairlis(other, ((argsPassed & 1) == 0) ? list() : aList);
    }

    // constants

    public static final Function CAR_FUNCTION =
        new Function() {
            public Object funcall(Object genericP) {
                Cons p = (Cons)genericP;
                return p.car();
            }};

    public static final Function CDR_FUNCTION =
        new Function() {
            public Object funcall(Object genericP) {
                Cons p = (Cons)genericP;
                return p.cdr();
            }};

    public static final Function REST_FUNCTION =
        new Function() {
            public Object funcall(Object genericP) {
                Cons p = (Cons)genericP;
                return p.rest();
            }};

    public static final Function SECOND_FUNCTION =
        new Function() {
            public Object funcall(Object genericP) {
                Cons p = (Cons)genericP;
                return p.second();
            }};

    public static final Function2 CONS_FUNCTION =
        new Function2() {
            public Object funcall(Object car, Object cdr) {
                return new Cons(car, cdr);
            }};

    public static final Function IDENTITY_FUNCTION =
        new Function() {
            public Object funcall(Object arg) {
                return arg;
            }};

    public static final Cons EMPTY_LIST = new Cons(null, null);

    // slots

    protected Object car;

    protected Object cdr;

    // static blocks

    static {
        EMPTY_LIST.setfCar(EMPTY_LIST);
        EMPTY_LIST.setfCdr(EMPTY_LIST);
    }
}
