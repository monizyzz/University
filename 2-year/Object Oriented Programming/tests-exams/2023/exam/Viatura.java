import java.util.Map;
import java.io.Serializable;
import java.util.HashMap;

public class Viatura implements Comparable<Viatura> , Serializable {
    private String matricula;
    private String nomeDono;
    private String tipoViatura; // indica de que tipo é a viatura: carro, mota, etc.
    private Map<String, Registo> meusEstacionamentos;

    public int compareTo(Viatura v){
        return 0;
    }
    public String getMatricula(){
        return this.matricula;
    }
     public String getTipoViatura(){
        return this.tipoViatura;
    }
    
    public Viatura(String matricula, String nomeDono, String tipoViatura){
        this.matricula = matricula;
        this.nomeDono = nomeDono;
        this.tipoViatura  = tipoViatura;
        this.meusEstacionamentos = new HashMap<String,Registo>();
    }
    
}
