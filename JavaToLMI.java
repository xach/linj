/// Copyright (C) eValuator, Lda

/// THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
/// OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
/// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
/// ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
/// DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
/// DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
/// GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
/// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
/// WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
/// NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
/// SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

import java.lang.reflect.*;
import java.io.*;

public class JavaToLMI {
    public static void main(String[] args) throws IOException {
        if (args.length == 0) { //Server mode
            String request;
            BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
            while ((request = in.readLine()) != null) {
                processRequest(request, false);
            }
        } else if (args.length == 1) { //One-shot mode
            processRequest(args[0], true);
        } else {
            System.err.println("Usage: JavaToLMI classname");
            System.exit(1);
        }
    }

    public static void processRequest(String name, boolean oneShotP) {
        try {
            dumpClassInfo(Class.forName(name, false, JavaToLMI.class.getClassLoader()));
            //Class.forName(name));
        } catch (ClassNotFoundException cnfe) {
            if (oneShotP) {
                System.err.println("Class '" + name + "' not found");
                System.exit(1);
            } else {
                println("()");
            }
        }
    }

    public static void dumpClassInfo(Class classToDump) {
        printsp(classToDump.isInterface() ? "(defmixin-0" : "(defclass-0");
        printsp(toTypeName(classToDump));

        if (! classToDump.isInterface()) {
            Class superclass = classToDump.getSuperclass();
            if (superclass == null) {
                printsp("()");
            } else {
                printsp(toTypeName(superclass));
            }
        }

        print("(");
        Class[] interfaces = classToDump.getInterfaces();
        for(int i = 0; i < interfaces.length; i++) {
            printsp(toTypeName(interfaces[i]));
        }
        println(")");

        println("()"); //options

        Field[] fields = classToDump.getDeclaredFields();
        for(int i = 0; i < fields.length; i++) {
            dumpFieldInfo(fields[i]);
        }

        Constructor[] constructors = classToDump.getDeclaredConstructors();
        for(int i = 0; i < constructors.length; i++) {
            dumpConstructorInfo(constructors[i]);
        }

        Method[] methods = classToDump.getDeclaredMethods();
        for(int i = 0; i < methods.length; i++) {
            if (! methods[i].isBridge()) {
                dumpMethodInfo(methods[i]);
            }
        }

//      Class[] classes = classToDump.getDeclaredClasses();
//      for(int i = 0; i < classes.length; i++) {
//          dumpClassInfo(classes[i]);
//      }

        println(")");
    }

    public static void dumpFieldInfo(Field field) {
        if (isPublicOrProtected(field)) {
            printsp("    (defslot-0");

            print("(");
            printsp(field.getName());
            printsp("(the");
            print(toTypeName(field.getType()));
            printsp("))");
//          print(field.getName());
//          print("/");
//          printsp(toTypeName(field.getType()));

            dumpMemberOptions(field);
            println(")");
        }
    }

    public static void dumpConstructorInfo(Constructor constructor) {
        if (isPublicOrProtected(constructor)) {
            printsp("    (defnew");
            dumpArglist(constructor.getParameterTypes(), false);
            dumpMemberOptions(constructor);
            printsp("");
            dumpThrows(constructor.getExceptionTypes());
            println(")");
        }
    }

    public static void dumpMethodInfo(Method method) {
        if (isPublicOrProtected(method)) {
            printsp("    (defmethod");
            //      printsp(toTypeName(method.getReturnType()));
            printsp(method.getName());
            dumpArglist(method.getParameterTypes(),
                        ! Modifier.isStatic(method.getModifiers()));
            dumpMemberOptions(method);
            printsp("");
            if (Modifier.isAbstract(method.getModifiers())) {
                printsp(":category :abstract");
            }

            printsp(" :returns");
            printsp(toTypeName(method.getReturnType()));

            dumpThrows(method.getExceptionTypes());
            println(")");
        }
    }

    public static void dumpMemberOptions(Member member) {
        int mods = member.getModifiers();
        if (Modifier.isStatic(mods)) {
            printsp(":allocation :class");
        }

        if (Modifier.isFinal(mods)) {
            printsp(":constant t");
        }

        printsp(":visibility");
        if (Modifier.isPublic(mods)) {
            print(":public");
        } else {
            print(":protected");
        }
    }

    public static void dumpArglist(Class[] argTypes, boolean withThis) {
        print("(");
        if (withThis) {
            printsp("this");
        }
        for(int i = 0; i < argTypes.length; i++) {
            print("(arg" + i + " (the ");
            print(toTypeName(argTypes[i]));
            printsp("))");
        }
        printsp(")");
    }

    public static void dumpThrows(Class[] excepTypes) {
        if (excepTypes.length > 0) {
            print(":throws (");
            for(int i = 0; i < excepTypes.length; i++) {
                printsp(toTypeName(excepTypes[i]));
            }
            printsp(")");
        }
    }

    public static boolean isPublicOrProtected(Member member) {
        int mods = member.getModifiers();
        return (Modifier.isPublic(mods) || Modifier.isProtected(mods));
    }

    public static String toTypeName(Class classObj) {
        return toPrintableName(classObj);
    }

    protected static String toPrintableName(Class classObj) {
        if (classObj.isArray()) {
            return toPrintableName(classObj.getComponentType()) + "[]";
        } else {
            return classObj.getName().replace('$', '/');
        }
    }

    protected static void println(String text) {
        System.out.println(text);
    }

    protected static void printsp(String text) {
        System.out.print(text);
        System.out.print(" ");
    }

    protected static void print(String text) {
        System.out.print(text);
    }
}
