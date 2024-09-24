import java.util.List;

public class Podcast {
    private String nome;
    private List<Episodio> episodios;

    public Podcast(String nome, List<Episodio> episodios) {
        this.nome = nome;
        this.episodios = episodios;
    }

    public String getNome() {
        return this.nome;
    }

    public List<Episodio> getEpisodios() {
        return this.episodios;
    }


}
