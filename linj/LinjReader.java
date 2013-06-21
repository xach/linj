package linj;

import java.io.*;
import java.lang.reflect.*;
import java.math.*;
import java.util.Hashtable;

public class LinjReader extends Object {

    // constructors

    public LinjReader(Reader in, String pkg) {
        reader = in;
        _package = (pkg.equals("")) ? pkg : pkg + ".";
    }

    // methods

    public Object pushToken(Object newToken) {
        if (pushedToken == null) {
            return pushedToken = newToken;
        } else {
            throw new Error("Token already pushed!!!");
        }
    }

    public Object popToken() {
        if (pushedToken == null) {
            throw new Error("No token available to pop!!!");
        } else {
            Object results = pushedToken;
            pushedToken = null;
            return results;
        }
    }

    public boolean pushedTokenP() {
        return pushedToken != null;
    }

    public int pushChar(int newChar) {
        if (pushedChar == NULL_CHAR) {
            return pushedChar = newChar;
        } else {
            throw new Error("Char already pushed!!!");
        }
    }

    public int popChar() {
        if (pushedChar == NULL_CHAR) {
            throw new Error("No char available to pop!!!");
        } else {
            int results = pushedChar;
            pushedChar = NULL_CHAR;
            return results;
        }
    }

    public boolean pushedCharP() {
        return ! (((char)pushedChar) == NULL_CHAR);
    }

    public Object read(Object eofValue) {
        try {
            Object token = readToken();
            if (token == CLOSE_PARENTHESIS) {
                System.err.print("Extra ')' ignored");
                return read(null);
            } else if (token == OPEN_PARENTHESIS) {
                return readTail(false);
            } else if (token == END_OF_FILE) {
                return eofValue;
            } else {
                return token;
            }
        } catch (IOException e) {
            System.err.print("Input error:");
            return Cons.EMPTY_LIST;
        }
    }

    public Object readTail(boolean dotOkP) throws IOException {
        Object token = readToken();
        if (token == END_OF_FILE) {
            throw new IOException("End of file during read");
        } else if (token == CLOSE_PARENTHESIS) {
            return Cons.list();
        } else if ((token == DOT) && dotOkP) {
            Object result = read(null);
            Object newToken = readToken();
            if (newToken == CLOSE_PARENTHESIS) {
                return result;
            } else {
                throw new IOException("Where's the ')'? Got " + newToken + " instead!");
            }
        } else if (token == DOT) {
            throw new IOException("'.' immediately after '('");
        } else {
            pushToken(token);
            Object car = read(null);
            Object cdr = readTail(true);
            return new Cons(car, cdr);
        }
    }

    public Object readToken() throws IOException {
        if (pushedTokenP()) {
            return popToken();
        } else {
            int ch = (pushedCharP()) ? popChar() : reader.read();
            for (; whitespaceP(ch); ch = reader.read()) {
            }
            switch (ch) {
            case '(':
                return OPEN_PARENTHESIS;
            case ')':
                return CLOSE_PARENTHESIS;
            case '"':
                return readString();
            case ';':
                readComment();
                return readToken();
            case '#':
                return readDispatch();
            case '\'':
                return Cons.list(Symbol.intern("quote"), read(null));
            case -1:
                return END_OF_FILE;
            default:
                return readNumberOrSymbol(ch);
            }
        }
    }

    public Object readDispatch() throws IOException {
        int dispatch = reader.read();
        switch (dispatch) {
        case 'S':
            return readStructure();
        case '(': {
            Object elems = readTail(false);
            return makeVectorFromList((Cons)elems);
        }
        default:
            pushChar(dispatch);
            sharedSyntax = true;
            Object key = readToken();
            int termChar = popChar();
            switch (termChar) {
            case '=': {
                sharedSyntax = false;
                Object shared = read(null);
                if (sharedTable.get(key) == null) {
                    sharedTable.put(key, shared);
                } else {
                    throw new Error("Duplicate read definition name #" + key);
                }
                return shared;
            }
            case '#':
                sharedSyntax = false;
                return sharedTable.get(key);
            default:
                throw new Error("Unknown terminating circle dispatch char #" + dispatch + termChar);
            }
        }
    }

    public static Object[] makeVectorFromList(Cons elems) {
        int size = elems.length();
        Object[] array = new Object[size];
        int i = 0;
        Cons listE = elems;
        for (; ! listE.endp(); listE = listE.rest()) {
            Object e = listE.first();
            array[i] = e;
            ++i;
        }
        return array;
    }

    public String readString() throws IOException {
        buffer.setLength(0);
        for (int ch = reader.read(); ! (((char)ch) == '"'); ch = reader.read()) {
            buffer.append((char)ch);
        }
        return buffer.toString();
    }

    public void readComment() throws IOException {
        for (int ch = reader.read(); (! (((char)ch) == '\n')) && (! (((char)ch) == '\r')); ch = reader.read()) {
        }
    }

    public Object readNumberOrSymbol(int ch) throws IOException {
        int c = ch;
        buffer.setLength(0);
        while (true) {
            buffer.append((char)ch);
            ch = reader.read();
            if (delimiterP(ch)) {
                break;
            }
        }
        pushChar(ch);
        if ((((char)c) == '.') && (buffer.length() == 1)) {
            return DOT;
        } else {
            String token = buffer.toString();
            if ((((char)c) == '.') ||
                (((char)c) == '+') ||
                (((char)c) == '-') ||
                ((((char)c) >= '0') && (((char)c) <= '9'))) {
                try {
                    return Bignum.valueOf(token);
                } catch (Exception e) {
                    try {
                        return new BigDecimal(token);
                    } catch (Exception e0) {
                        return Symbol.intern(token);
                    }
                }
            } else {
                return Symbol.intern(token);
            }
        }
    }

    public Object readStructure() throws IOException {
        return parseStructure((Cons)read(null));
    }

    public Object parseStructure(Cons structDescr) throws IOException {
        String className = linjNameToJavaTypeName(_package, (Symbol)structDescr.first());
        try {
            Class _class = Class.forName(className);
            Method method = _class.getMethod("parse", new Class[] { Class.forName("linj.Cons") });
            method.setAccessible(true);
            return method.invoke(null, new Cons[] { structDescr });
        } catch (InvocationTargetException ite) {
            ite.getTargetException().printStackTrace();
            throw new IOException("Couldn't parse the structure #S" + structDescr);
        } catch (Exception e) {
            System.err.print(e);
            e.printStackTrace();
            throw new IOException("Couldn't parse the structure #S" + structDescr);
        }
    }

    public static String linjNameToJavaTypeName(String _package, Symbol linjName) {
        String name = linjName.toString();
        StringBuffer javaName = new StringBuffer(_package);
        char previous = '-';
        int limit = name.length();
        for (int i = 0; i < limit; ++i) {
            char current = name.charAt(i);
            if (! (current == '-')) {
                javaName.append((previous == '-') ? Character.toUpperCase(current) : current);
            }
            previous = current;
        }
        return javaName.toString();
    }

    public boolean delimiterP(int ch) {
        if (sharedSyntax) {
            switch (ch) {
            case '=':
            case '#':
                return true;
            default:
                return false;
            }
        } else {
            switch (ch) {
            case '(':
            case ')':
            case '\'':
            case '"':
            case ' ':
            case '\t':
            case '\n':
            case '\r':
            case -1:
                return true;
            default:
                return false;
            }
        }
    }

    public boolean whitespaceP(int ch) {
        switch (ch) {
        case ' ':
        case '\t':
        case '\n':
        case '\r':
            return true;
        default:
            return false;
        }
    }

    public static void main(String[] outsideArgs) {
        LinjReader r = new LinjReader(new InputStreamReader(System.in), "");
        while (true) {
            System.out.print("|");
            System.out.print(r.read(null));
            System.out.println("|");
        }
    }

    // constants

    public static final char NULL_CHAR = ' ';

    public static final Symbol DOT = Symbol.intern(".");

    public static final Symbol DOT_COMMA = Symbol.intern(";");

    public static final Symbol OPEN_PARENTHESIS = Symbol.intern("(");

    public static final Symbol CLOSE_PARENTHESIS = Symbol.intern(")");

    public static final Symbol END_OF_FILE = Symbol.intern("THIS IS THE END OF FILE");

    // slots

    protected Reader reader;

    protected StringBuffer buffer = new StringBuffer();

    protected Hashtable sharedTable = new Hashtable();

    protected boolean sharedSyntax = false;

    protected Object pushedToken;

    protected int pushedChar = (int)NULL_CHAR;

    protected String _package = "";
}
