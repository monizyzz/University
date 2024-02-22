import java.time.LocalDate;
import java.util.Scanner;
import static java.lang.System.out;

public class Ficha2 {

    public static void main(String[] args) {
        Scanner scan = new Scanner(System.in);

        out.print("Exercício: ");
        int ex = scan.nextInt();
        int i,j;

        switch (ex) {
            case 1:
                // inicializar o array
                Array array = new Array();
                out.print("Alínea: ");
                String alinea1 = scan.next();
                out.print("Quantos inteiros quer introduzir: ");
                int numero_Inteiros = scan.nextInt();
                switch (alinea1) {
                    case "a":
                        int minimo = array.min(numero_Inteiros);
                        out.println("min: " + minimo);
                        break;
                    case "b":
                        array.devolverEntreIndices(numero_Inteiros);
                        break;
                    case "c":
                        int[] comuns = new int[numero_Inteiros];
                        array.comumAosDois(numero_Inteiros, comuns);
                        break;
                }
                break;

        }
    }
}