import java.util.Scanner ;

public class CSupport {

    private static Scanner scanner = new Scanner(System.in) ;

    public static int readInt() {
        return scanner.nextInt() ;
    }

    public static void printInt(int i) {
        System.out.println(i) ;
    }

    public static boolean readBool() {
        return scanner.nextBoolean() ;
    }

    public static void printBool(boolean i) {
        System.out.println(i) ;
    }

    public static double readDouble() {
        return scanner.nextDouble() ;
    }

    public static void printDouble(double x) {
        System.out.println(x) ;
    }

    public static String readString() {
        return scanner.next() ;
    }

    public static void printString(String s) {
        System.out.println(s) ;
    }

    public static String concat(String s, String t) {
        StringBuilder sb = new StringBuilder(s) ;
        sb.append(t) ;
        return sb.toString() ;
    }
}
