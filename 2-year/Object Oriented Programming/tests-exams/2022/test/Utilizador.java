import java.io.Serializable;
import java.time.LocalDate;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

public class Utilizador implements Serializable {
    private String numUser;
    private String nomeUser;
    private LocalDate dataAdesao; // data de adesão do utilizador à aplicação
    private Set<Livro> colecaoLivros;


    public Utilizador(String numUser, String nomeUser, Iterator<Livro> livros) {
        this.numUser = numUser;
        this.nomeUser = nomeUser;
        this.dataAdesao = LocalDate.now();
        this.colecaoLivros = new TreeSet<>();
        while(livros.hasNext()) {
            this.colecaoLivros.add(livros.next().clone());
        }
    }

    public void avancaPags(String codISBN, int n) throws Exception {
        Livro livro = null;

        for (Livro l: this.colecaoLivros) {
            if (l.getCodISBN().equals(codISBN)) {
                livro = l.clone();
                while (n > 0) {
                    Pagina p = livro.getPagPorLer().get(0);
                    livro.setPagLidas(getPagLidas().add(p));
                    livro.setPagPorLer(getPagPorLer().remove(0));
                    n--;
                }

            }
        }

        if (livro == null) {
            throw new Exception("erro avancar paginas");
        }
    }

    public List<String> reproduzLivros() {
        
    }

}