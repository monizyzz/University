import java.util.Scanner;
import static java.lang.System.out;

public class App {
    public static void main(String[] args) {
        Scanner scan = new Scanner(System.in);
        Ficha1 ficha1 = new Ficha1();
        out.print("Número do exercício: ");
        int ex = scan.nextInt();

        switch (ex) {
            case 1:
                out.print("Graus Celsius: ");
                double graus = scan.nextInt();
                double farenheit = ficha1.celsiusParaFarenheit(graus);
                out.println(graus + " graus celsius correspondem a " + farenheit + " graus Farenheit");
                break;
            
                case 2:
                out.print("Número 1: ");
                int num1 = scan.nextInt();
                out.print("Número 2: ");
                int num2 = scan.nextInt();
                int maximo = ficha1.maximoNumeros(num1, num2);
                out.println(maximo + " é o maior entre " + num1 + " e " + num2);
                break;

            case 3:
                out.print("Insira o nome: ");
                String nome = scan.next();
                out.print("Insira o saldo: ");
                double saldo = scan.nextDouble();
                String res = ficha1.criaDescricaoConta(nome, saldo);
                out.println(res);
                break;
            
            case 4:
                out.print("Insira o valor em Euros: ");
                double valor = scan.nextDouble();
                out.print("Insira a taxa de conversão para libras: ");
                double taxaConversao = scan.nextDouble();
                double euroParaLibras = ficha1.eurosParaLibras(valor, taxaConversao);
                out.println(valor + " euros convertido para libras é equivalente " + euroParaLibras + " libras");
                break;

            case 5:
                out.print("Insira um inteiro: ");
                int a = scan.nextInt();
                out.print("Insira outro inteiro: ");
                int b = scan.nextInt();
                float media = (a + b) / 2;
                out.println("Maior: " + Math.max(a, b) + " | Menor: " + Math.min(a, b) + " | Media: " + media);
                break;

            case 6:
                out.print("Insira um inteiro: ");
                int x = scan.nextInt();
                out.println("O fatorial de " + x + " é " + ficha1.factorial(x));
                break;

            case 7:
                out.println("Total de milisegundos que o fatorial demorou a executar: " + ficha1.tempoGasto());
                break;

            default:
                out.print("Exercício não existente");
        }

        scan.close();

    }
}