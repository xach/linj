package linj;

import java.util.Hashtable;

public class Symbol extends Object {

    // constructors

    public Symbol(String name) {
        this.name = name;
    }

    // accessors

    public Object symbolValue() {
        return value;
    }

    public Object setfSymbolValue(Object value) {
        return this.value = value;
    }

    public Cons symbolPlist() {
        return plist;
    }

    public Cons setfSymbolPlist(Cons plist) {
        return this.plist = plist;
    }

    // methods

    public static Symbol intern(String name) {
        Symbol sym = (Symbol)SYMBOLS.get(name);
        if (sym == null) {
            Symbol newSym = new Symbol(name);
            SYMBOLS.put(name, newSym);
            return newSym;
        } else {
            return sym;
        }
    }

    public static Symbol findSymbol(String name) {
        return (Symbol)SYMBOLS.get(name);
    }

    public Object unintern() {
        return SYMBOLS.remove(name);
    }

    public Cons copySymbol(boolean copyProps) {
        Symbol sym = new Symbol(name);
        sym.setfSymbolValue(value);
        return sym.setfSymbolPlist((copyProps) ? plist.copyList() : plist);
    }

    public boolean equals(Object other) {
        if (other instanceof String) {
            throw new Error("Probably a semantic error that would go unnoticed if we didn't BARF now!");
        }
        if (! (other instanceof Symbol)) {
            return false;
        } else {
            return name.equals(name);
        }
    }

    public int hashCode() {
        return name.hashCode();
    }

    public String toString() {
        return name;
    }

    public boolean keywordp() {
        return name.charAt(0) == ':';
    }

    public String symbolName() {
        if (keywordp()) {
            return name.substring(1);
        } else {
            return name;
        }
    }

    public Object set(Object value) {
        return setfSymbolValue(value);
    }

    public Object get(Object indicator, Object _default) {
        return symbolPlist().getf(indicator, _default);
    }

    public Cons setfGet(Object value, Object indicator, Object _default) {
        return setfSymbolPlist(symbolPlist().putPlist(indicator, value));
    }

    public Cons remprop(Object indicator) {
        return setfSymbolPlist(symbolPlist().remPlist(indicator));
    }

    public static Symbol gensym() {
        return gensym("G");
    }

    public static Symbol gensym(String prefix) {
        int old = starGensymCounterStar;
        starGensymCounterStar = old + 1;
        return new Symbol(prefix + String.valueOf(old));
    }

    public static Symbol gensym(int suffix) {
        return new Symbol("G" + String.valueOf(suffix));
    }

    // constants

    public static final Hashtable SYMBOLS = new Hashtable();

    // slots

    protected String name;

    protected Object value;

    protected Cons plist = Cons.list();

    public static int starGensymCounterStar = 0;
}
