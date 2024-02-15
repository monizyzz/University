/**
 * Calcular o somatório de n números enquanto
 * o utilizador assim o quiser.
 */

import java.util.Scanner;

public class Ex4 {
    public static void main(String [] args){
        int soma, n;
        String resp;
        Scanner input = new Scanner(System.in);

        do {
            System.out.println("Quantos números vai somar? ");
            n = input.nextInt();
            soma = 0;
            for (int  i = 0; i < n; i++) {
                System.out.printf("Valor: ");
                soma += input.nextInt();
            }

            System.out.println("O somatório é: " + soma);
            System.out.println("Quer repetir? [S/n]");
            resp = input.next();
        
        } while (resp.charAt(0) != 'n');

        input.close();
    }
}