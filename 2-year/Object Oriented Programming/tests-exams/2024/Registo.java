import java.io.Serializable;
import java.time.LocalDate;

public class Registo implements Comparable<Registo>, Serializable {
    private Integer numRegisto;
    private LocalDate dataInicio;
    private LocalDate dataFim;
    private Quarto quarto;

    public Registo(LocalDate dataInicio, LocalDate dataFim, Quarto quarto) {
        this.dataInicio = dataInicio;
        this.dataFim = dataFim;
        this.quarto = quarto;
    }
    
    public Registo(Integer numRegisto, LocalDate dataInicio, LocalDate dataFim, Quarto quarto) {
        this.numRegisto = numRegisto;
        this.dataInicio = dataInicio;
        this.dataFim = dataFim;
        this.quarto = quarto;
    }

    public Integer getnumRegisto() {
        return numRegisto;
    }

    public Quarto getQuarto() {
        return quarto;
    }

    public LocalDate getDataInicio() {
        return dataInicio;
    }

    public LocalDate getDataFim() {
        return dataFim;
    }

    public void setQuarto(Quarto q) {
        this.quarto = q;
    }

    public int compareTo(Registo r) {
        return 0;
    } // compara dois registos de ocupação
    
    
    public int numDiasReserva() {
        return 0;
    } // devolve o número de dias deste
    // registo de ocupação do quarto
    public double valorAPagar() {
        return 0.0;
    } // determina o valor a pagar pela
    // ocupação do quarto, tendo em conta
    // os descontos que se decida aplicar

}