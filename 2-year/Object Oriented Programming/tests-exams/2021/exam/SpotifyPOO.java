import java.util.ArrayList;
import java.util.List;

public class SpotifyPOO {
    private List<Podcast> podcasts;
    private List<User> users;
    // private Map<String,<Podcast>> podcasts;

    // questao 6 
    public List<Episodio> getEpisodios(String nomePodcast) {
        for (Podcast podcast : this.podcasts) {
            if (podcast.getNome().equals(nomePodcast)) {
                return new ArrayList<>(podcast.getEpisodios());
            }
        }
        return null;
    }

    // questao 8
    public void remove(String nomeP) throws Exception {
        Podcast pod = null;

        for (Podcast p : this.podcasts) {
            if (p.getNome().equals(nomeP)) {
                pod = p;
                break;
            }
        }
        
        if (pod == null) {
            throw new Exception("podcast nao encontrado");
        }
        
        for (User u : this.users) {
            if (u.getPodcastsSubscritos().equals(pod)) {
                
            }
        }
        this.podcasts.remove(pod);
    }

}
