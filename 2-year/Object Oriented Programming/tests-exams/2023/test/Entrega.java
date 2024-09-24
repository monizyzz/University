import java.io.Serializable;
import java.time.*;
import java.time.LocalDate;

public class Entrega implements Comparable<Entrega>, Serializable {
    private LocalDate data;
    private int nota_docente;
    private Aluno avaliador;
    private int nota_avaliador;
    private String comentarios;
    public int calculaNotaEntrega() {
        return 0; 
    } //método que calcula a nota desta entrega
    public int compareTo(Entrega e ){
        return 0;
    }
    public Aluno getAluno(){
        return this.avaliador;
    }
    public LocalDate getDataLimite(){
        return this.data;
        
    }

}