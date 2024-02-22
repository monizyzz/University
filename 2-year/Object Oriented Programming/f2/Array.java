import java.util.Arrays;
import java.util.Scanner;
import static java.lang.System.out;

public class Array {
    Scanner scan = new Scanner(System.in);
    // 1. (a)
    public int min(int size) {
        int[] array = new int[size];
        int res = array[0];
        
        for (int i = 0; i < size; i += 1) {
            out.print("Insere número: ");
            int numero = scan.nextInt();
            array[i] = numero;
        }
        
        for (int j = 0; j < size; j += 1) {
            if (array[j] < res) {
                res = array[j];
            }
        }
        return res;
    }

    // 1. (b)
    public void devolverEntreIndices(int size) {
        int[] array = new int[size];
        
        for (int i = 0; i < size; i += 1) {
            out.print("Insere número: ");
            int numero = scan.nextInt();
            array[i] = numero;
        }

        out.print("Insere o primeiro índice: ");
        int fst_index = scan.nextInt();
        out.print("Insere o segundo índice: ");
        int snd_index = scan.nextInt();

        if (fst_index < 0 || snd_index >= size) {
            out.print("Index out of bound");
        }

        int[] mid = new int[snd_index - fst_index];
 
        for (int j = 0; j < snd_index - fst_index; j += 1) {
            mid[j] = array[fst_index + j];
            out.println(mid[j]);
        }
    }

    // 1. (c)
    public void comumAosDois(int size, int[] comuns){
        int[] array1 = new int[size];
        int[] array2 = new int[size];
        
        for(int i = 0; i < size; i += 1){
            out.print("Array1 posição " + i + ": ");
            array1[i] = scan.nextInt();
        }
        out.println();
        
        for(int i = 0; i < size; i += 1){
            out.print("Array2 posição " + i + ": ");
            array2[i] = scan.nextInt();
        }
        
        Arrays.sort(array1);
        Arrays.sort(array2);
        
        
        int j = 0;
        for (int i = 0; i < size; i++) {
            if (Arrays.binarySearch(array2, array1[i]) >= 0) {
                comuns[j] = array1[i];
                out.print(comuns[j]);
                j++;
            }
        }
        out.println();
    }
}