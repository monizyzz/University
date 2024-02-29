import java.time.LocalDate;
import java.util.Scanner;
import static java.lang.System.out;

public class Ficha2 {

    public static void main(String[] args) {
        Scanner scan = new Scanner(System.in);

        out.print("Exercício: ");
        int ex = scan.nextInt();

        switch (ex) {
            case 1:
                // inicializar o array
                Array array = new Array();
                out.print("Alínea: ");
                String alinea1 = scan.next();
                out.print("Quantos inteiros quer introduzir: ");
                int numero_Inteiros = scan.nextInt();
                out.println();

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
            
            case 2:
                out.print("Tamanho do array das datas: ");
                int size = scan.nextInt();
                Datas datas = new Datas(size);
                out.print("Alínea: ");
                String alinea2 = scan.next();
                int d, m, a;
                out.println();

                switch (alinea2) {
                    case "a":
                        System.out.print("Digite o dia: ");
                        d = scan.nextInt();
                        System.out.print("Digite o mes: ");
                        m = scan.nextInt();
                        System.out.print("Digite o ano: ");
                        a = scan.nextInt();
                        datas.insereData(LocalDate.of(a,m,d));
                        break;
                            
                    case "b":
                        System.out.print("Digite o dia: ");
                        d = scan.nextInt();
                        System.out.print("Digite o mes: ");
                        m = scan.nextInt();
                        System.out.print("Digite o ano: ");
                        a = scan.nextInt();
                        LocalDate data = LocalDate.of(a,m,d);
                        out.print("A data mais próxima é " + datas.dataMaisProxima(data));
                        break;
                        
                    case "c":
                        out.print(datas.toString());
                        break;
                        
                    default:
                        out.println("Alínea inexistente");
                        break;
                }
            case 6:
                Matrizes matrizes = new Matrizes();
                int numLinhas = 3;
                int numColunas = 3;
                int[][] matriz1 = {{5,1,7},
                                   {6,8,4},
                                   {3,2,9}};

                int[][] matriz2 = {{52,13,76},
                                   {66,81,47},
                                   {39,24,95}};
                int[][] result = new int[numLinhas][numColunas];


                out.print("Alínea: ");
                String alinea6 = scan.next();
                out.println();
                
                switch (alinea6){
                    case "a":
                        matrizes.lerMatriz(matriz1, numLinhas, numColunas);
                        matrizes.lerMatriz(matriz2, numLinhas, numColunas);
                        break;
                    
                    case "b":
                        matrizes.somaMatrizes(matriz1, matriz2, result, numLinhas, numColunas);
                        break;
                    
                    case "c":
                        out.println(matrizes.saoIguais(matriz1, matriz2, numLinhas, numColunas));
                        break;
                    
                        case "d":
                        matrizes.matrizOposta(matriz1, result, numLinhas, numColunas);
                        matrizes.matrizOposta(matriz2, result, numLinhas, numColunas);
                        break;

                    default:
                        out.print("Alinea inexistente");
                        break;
                } 
        }

        scan.close();
    }
}