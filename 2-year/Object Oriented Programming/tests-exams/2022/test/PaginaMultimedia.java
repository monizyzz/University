import java.util.List;

public class PaginaMultimedia {
    private List<String> texto;
    private List<Byte> audio;
    private List<Byte> video;
    
    public PaginaMultimedia(List<String> texto, List<Byte> audio, List<Byte> video) {
        this.texto = texto;
        this.audio = audio;
        this.video = video;
    }

    /**
    * método que devolve uma formatação do texto, audio e vídeo.
    * Está devidamente implementado.
    */
    public String fazPagina() {
        return "";
    }

    
}
