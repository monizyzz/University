import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

public class Pagina implements Comparable<Pagina>, Serializable {
    private List<String> texto;

    public Pagina() {
        this.texto = new ArrayList<>();
    }

    public int compareTo(Pagina p){
        return 0;
    }

    public Pagina(List<String> texto) {
        this.texto = new ArrayList<>(texto);
    }

    /* método que devolve uma formatação do texto */
    public String reproduzPagina() {
        StringBuilder sb = new StringBuilder();
        for (String s : this.texto) {
            sb.append(s).append("\n");
        }
        return sb.toString();
    }
}