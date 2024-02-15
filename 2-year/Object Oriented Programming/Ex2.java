/**
 * Lê dois números e diz qual o maior.
 * (agora, sem inventar a roda!)
 */

import java.util.Scanner;

public class Ex2 {
    /** Diz qual é o maior − utilizando a classe Math */
    public static void  dizMaior (int i1, int i2) {
        System.out.println("O maior é " + Math.max(i1, i2));
    }

    public static void  main(String [] args) {
        int a, b;

        Scanner ler = new Scanner(System.in);
        System.out.print("Indique dois inteiros: ");
        
        a = ler.nextInt ();
        b = ler.nextInt ();
        
        dizMaior(a, b); // ou Ex2.dizMaior(a,b)
        
        ler.close();
    }
}