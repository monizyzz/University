/**
 * Lê dois números e diz qual o maior.
 */

import java.util.Scanner;

public class Ex1 {
    /** Diz qual é o maior de dois números */
    public static void dizMaior ( int i1 , int i2) {
        if (i1 > i2)
            System.out.println( "O maior é " + i1);
        else
            System.out.println( "O maior é " + i2);
    }

    public static void main (String [] args) {
        int a, b;

        Scanner ler = new Scanner(System.in);
        System.out.print("Indique dois inteiros: ");
        
        a = ler.nextInt ();
        b = ler.nextInt ();
        
        dizMaior(a, b); // ou Ex1.dizMaior(a,b)
        
        ler.close();
    }
}