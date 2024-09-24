import java.io.Serializable;
import java.util.List;
import java.util.ArrayList;

public class PaginaComAudio extends Pagina implements Comparable<PaginaComAudio>, Serializable {
    private String narrador;
    private List<Byte> som;
    
    public int compareTo(PaginaComAudio p){
        return 0;
    }

    public PaginaComAudio(List<String> texto, String narrador, List<Byte> audio) {
        super(texto);
        this.narrador = narrador;
        this.som = new ArrayList<>(audio);
    }
    /* método que devolve uma formatação do texto e audio */
    public String reproduzPagina() {
        StringBuilder sb = new StringBuilder();
        sb.append("Narrador: ").append(this.narrador).append("\n");
        sb.append(super.reproduzPagina());
        return sb.toString();
    }
}