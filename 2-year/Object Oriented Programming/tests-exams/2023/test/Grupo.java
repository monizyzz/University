import java.io.Serializable;
import java.util.*;

public class Grupo implements Comparable<Grupo>, Serializable {
    private String codGrupo;
    private List<Entrega> entregas;
    public Grupo(String codGrupo) {
    }
    public void addEntrega(Entrega e) {
    }
    public int calculaNotaGrupo() { 
        return 0; 
    }
    public int compareTo(Grupo g){
        return 0;
    }
    public List<Entrega> getEntregas(){
        return this.entregas;
    }
    public String getCodGrupo(){
        return this.codGrupo;
    }
}