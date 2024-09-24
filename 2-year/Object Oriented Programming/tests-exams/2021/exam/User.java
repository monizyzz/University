import java.util.List;

public class User {
    private String email;
    private String nome;
    private List<Podcast> podcastsSubscritos;

    public User(String email, String nome, List<Podcast> podcastsSubscritos) {
        this.email = email;
        this.nome = nome;
        this.podcastsSubscritos = podcastsSubscritos;
    }

    public String getEmail() {
        return this.email;
    }

    public String getNome() {
        return this.nome;
    }

    public List<Podcast> getPodcastsSubscritos() {
        return this.podcastsSubscritos;
    }
}
