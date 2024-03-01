import java.util.Random;
import static java.lang.System.out;

public class EuroMilhoes {
    private int[] numeros;
    private int[] estrelas;

    public EuroMilhoes (int numNumeros, int numEstrelas) {
        numeros = new int[numNumeros];
        estrelas = new int[numEstrelas];
    }

    public void gerarChave() {
        Random r = new Random();

        for (int i = 0; i < 5; i += 1) {
            numeros[i] = r.nextInt(50) + 1;
        }

        for (int j = 0; j < 2; j += 1) {
            estrelas[j] = r.nextInt(12) + 1;
        }

    }

    public void imprimeChave() {
        out.println("Chave do EuroMilhões");
        out.println("Números");

        for (int i = 0; i < 5; i += 1) {
            for (int j = 0; j < i; j += 1) {
                out.print("  ");
            }
            out.println(numeros[i]);
        }

        out.println("Estrelas");
        for (int a = 0; a < 2; a += 1) {
            for (int b = 0; b < a ; b += 1) {
                out.print("  ");
            }
            out.println(estrelas[a]);
        }
    }

    public void comparaAposta(int[] numerosUser, int[] estrelasUser) {
        int numGuessed = 0;
        int estrelasGuessed = 0;

        for (int i = 0; i < 5; i += 1) {
            for (int j = 0; j < 5; j += 1) {
                if (this.numeros[i] == numerosUser[j]) {
                    numGuessed += 1;
                    break;
                }
            }
        }

        for (int a = 0; a < 2; a += 1) {
            for (int b = 0; b < 2; b += 1) {
                if (this.estrelas[a] == estrelasUser[b]) {
                    estrelasGuessed += 1;
                    break;
                }
            }
        }

        out.println();
        if (numGuessed != 0 && estrelasGuessed != 0) {
            out.println("Acertou em " + numGuessed + " números e em " + estrelasGuessed + " estelas!");
        
        } else if (numGuessed != 0 && estrelasGuessed == 0) {
            out.println("Acertou em " + numGuessed + " número, mas não acertou em estrelas");
            
        } else if (numGuessed == 0 && estrelasGuessed != 0) {
            out.println("Acertou em " + estrelasGuessed + " estrelas, mas não acertou em números");
            
        } else {
            out.println("Não acertou em números nem estrelas u_u'");
        }
    }
}