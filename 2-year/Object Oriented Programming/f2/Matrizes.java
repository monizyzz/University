import static java.lang.System.out;

public class Matrizes {
    // 6. (a)
    public void lerMatriz (int[][] matrizes, int numLinhas, int numColunas) {
        for (int i = 0; i < numLinhas; i += 1) {
            for (int j = 0; j < numColunas;  j += 1) {
                out.print(matrizes[i][j] + " ");
            }
            out.println();
        }
    }

    // 6. (b)
    public void somaMatrizes(int[][] matriz1, int[][] matriz2, int [][] result, int numLinhas, int numColunas) {
        for (int i = 0; i < numLinhas; i += 1) {
            for (int j = 0; j < numColunas; j += 1) {
                result[i][j] = matriz1[i][j] + matriz2[i][j];
                out.print(result[i][j] + " ");
            }
            out.println();
        }
    }

    // 6. (c)
    public Boolean saoIguais(int[][] matriz1, int[][] matriz2, int numLinhas, int numColunas) {
        for (int i = 0; i < numLinhas; i += 1) {
            for (int j = 0; j < numColunas; j += 1) {
                if (matriz1[i][j] != matriz2[i][j]) {
                    return false;
                }
            }
        } 
        return true;
    }
    

    // 6. (d)
    public void matrizOposta(int[][] matriz1, int[][] result, int numLinhas, int numColunas) {
        out.println("Matriz oposta");
        for (int i = 0; i < numLinhas; i += 1) {
            for (int j = 0; j < numColunas; j += 1) {
                result[i][j] = -matriz1[i][j];
                out.print(result[i][j] + " ");
            }
            out.println();
        }
        out.println();
    }
}