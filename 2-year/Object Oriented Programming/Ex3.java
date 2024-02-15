/**
 * Calcular o somatório de 10 números
 */

import java.util.Scanner;

public class Ex3 {
    public static void main(String[] args) {
        int soma = 0;
        Scanner input = new Scanner(System.in);

        for (int i = 0; i < 10; i++) {
            System.out.print("Valor: ");
            soma += input.nextInt();
        }
        System.out.println("O somatório é: " + soma);
        input.close();
    }
}