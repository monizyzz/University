import java.io.Serializable;
import java.util.List;
import java.util.Map;

public abstract class Veiculo implements Comparable<Veiculo>, Serializable {
    private String matricula;
    private String marca;
    private String modelo;
    private double precokm; // preço base por km
    private List<Viagem> viagens;
    
    public abstract float custoViagem(float distancia);


    

}