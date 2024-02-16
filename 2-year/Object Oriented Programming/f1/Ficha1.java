import java.time.LocalDateTime;

import static java.lang.System.nanoTime;
import static java.lang.System.out;

public class Ficha1 {
    // 1.
    public double celsiusParaFarenheit(double graus) {
        return((graus * 1.8) + 32);
    }

    // 2.
    public int maximoNumeros(int a, int b) {
        return(Math.max(a,b));
    }

    // 3.
    public String criaDescricaoConta(String nome, double saldo) {
        return("Nome: " + nome + " | Saldo: " + saldo);
    }

    // 4.
    public double eurosParaLibras(double valor, double taxaConversao) {
        return(valor * taxaConversao);
    }

    // 5. na main 
    
    // 6.
    public long factorial(int num) {
        long res = 1;
        for(int i = 1; i <= num; i++) {
            res *= i;
        }
        return(res);
    }

    // 7.
    public long tempoGasto() {
        long before = System.nanoTime();
        out.println(factorial(5000));
        long after = System.nanoTime();
        
        return((after - before) / 1000000);
    }
}