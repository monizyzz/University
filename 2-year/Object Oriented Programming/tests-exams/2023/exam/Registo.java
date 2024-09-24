import java.time.LocalDateTime;
import java.io.Serializable;
import java.time.Duration;


public class Registo implements Comparable<Registo>, Serializable {
    private String numRegisto;
    private Parque parque;
    private Viatura carro;
    private LocalDateTime horaEntrada;
    private LocalDateTime horaSaida; // só deverá ser preenchida quando a viatura sai do parque
    
    public Registo(Parque parque, Viatura carro) {
        this.parque = parque;
        this.carro = carro;
        this.horaEntrada = LocalDateTime.now();
    } // cria um registo com a hora actual no parque indicado
    public double tempoNoParque() {
        if(horaSaida != null){
        Duration d = Duration.between(horaSaida,horaEntrada);
        return d.getSeconds();
        } else return 0;
    } // devolve o número de horas de estacionamento
    
    public int compareTo(Registo r){
        return 0;
    }
    public LocalDateTime getHoraEntrada(){
        return this.horaEntrada;
    }
    public LocalDateTime getHoraSaida(){
        return this.horaSaida;
    }
    public Viatura getViatura(){
        return this.carro; 
    }
    public void setHoraSaida(LocalDateTime horaSaida){
        this.horaSaida = horaSaida;
    }
}