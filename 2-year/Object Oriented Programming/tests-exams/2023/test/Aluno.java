import java.util.*; 
import java.io.Serializable;

public class Aluno implements Comparable<Aluno>, Serializable {
    private String codAluno;
    private String nomeAluno;
    private Grupo meuGrupo;
    private int notaTeorica;
    private int notaPratica;
    public void regista(Grupo g) {
    }
    public int calculaNotaFinal() {
        return 0;
    } 
    public int compareTo(Aluno a ){
        return 0;
    }// calcula a nota final de um aluno
    public String getCodAluno(){
        return this.codAluno;
    }
}