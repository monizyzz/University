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
                break;

            case 5: 
                Turma turma = new Turma(5,5);
                // 5. (a)
                // preencher matriz para depois utilizar outras alíneas
                for (int i = 0; i < 5; i += 1) {
                    out.println("-- ALUNO " + i + " --");
                    out.println();
                    for (int j = 0; j < 5; j += 1) {
                        out.println("Uc " + j);
                        out.print("Nota: ");
                        int nota = scan.nextInt();
                        turma.atualizarPauta(i, j, nota);
                    }
                    out.println();
                }
                do {
                    out.print("Alínea: ");
                    String alinea5 = scan.next();
                    out.println();


                    switch (alinea5) {
                        case "b":
                            out.print("Índice da Uc: ");
                            int indiceUc = scan.nextInt();
                            int somaNotas = turma.somaNotasUcs(indiceUc);
                            out.println("A soma das notas desta uc é " + somaNotas);
                            break;

                        case "c":
                            out.print("Índice do aluno: ");
                            int indiceAluno = scan.nextInt();
                            float mediaNotasAluno = turma.mediaAluno(indiceAluno);
                            out.println("A média deste aluno é " + mediaNotasAluno);
                            break;

                        case "d":
                            out.print("Insira o índice da uc: ");
                            int ucIndex = scan.nextInt();
                            float mediaNotasUc = turma.mediaUc(ucIndex);
                            out.println("A média desta uc é " + mediaNotasUc);
                            break;

                        case "e":
                            int notaMaisAlta = turma.notaMaisAlta();
                            out.println("A nota mais alta da turma é " + notaMaisAlta);
                            break;

                        case "f":
                            int notaMaisBaixa = turma.notaMaisBaixa();
                            out.println("A nota mais baixa da turma é " + notaMaisBaixa);
                            break;

                        case "g":
                            out.println("Insira um numero inteiro: ");
                            int valor = scan.nextInt();
                            turma.notasAcimaDe(valor);
                            break;

                        case "h":
                            out.println("Notas de todos os alunos do curso");
                            turma.notasTodosAlunos();
                            break;

                        case "i":
                            turma.mediaUcMaisAlta();
                            break;
                    
                        default:
                            out.println("Alínea inexistente");
                    }

                    out.print("Deseja continuar (s/n)? ");

                } while (scan.next().equalsIgnoreCase("s"));
                break;

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
                
                switch (alinea6) {
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
                        out.print("Alínea inexistente");
                        break;
                }
                break;

            case 7: 
                out.println("EuroMilhões");
                EuroMilhoes euroMilhoes = new EuroMilhoes(5,2);
                euroMilhoes.gerarChave();
                int[] numerosUser = new int[5];
                int[] estrelasUser = new int[2];

                out.println("Insere números entre 1 e 50");
                for (int i = 0; i < 5; i += 1) {
                    out.print("Número " + (i+1) + ": ");
                    numerosUser[i] = scan.nextInt();
                }

                out.println("Insere números entre 1 e 12");
                for (int i = 0; i < 2; i += 1) {
                    out.print("Estrela " + (i+1) + ": ");
                    estrelasUser[i] = scan.nextInt();
                }

                euroMilhoes.imprimeChave();
                euroMilhoes.comparaAposta(numerosUser, estrelasUser);
                break;
        } 

        scan.close();
    }
}