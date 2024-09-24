import java.util.List;
import java.io.Serializable;

public class Livro implements Comparable<Livro>, Serializable {
    public String codISBN; //código ISBN do livro
    private String nomeLivro;
    private String autor;
    private String editora;
    private List<Pagina> pagLidas; // páginas já lidas
    private List<Pagina> pagPorLer; //páginas ainda por ler.
                                    //o primeiro elemento é a página a ser lida no momento

    /* método que devolve a página com o número indicado */
    public Pagina devolvePag(int numPag) throws PagInexistenteException {
        Pagina res = null;
        int numLidas = this.pagLidas.size(); //número de páginas lidas
        int porLer = this.pagPorLer.size();

        if (numPag > numLidas+porLer)
            throw new PagInexistenteException(numLidas);
        if (numPag <= numLidas )
            res = this.pagLidas.get(numPag -1);
        else
            res = this.pagPorLer.get(numPag-numLidas -1);
        return res.clone();
    }

    public int compareTo(Livro p){
        return 0;
    }

    public Livro livroMaisLido() {
        Livro livroMaisLido = null;
        int maxPaginasLidas = 0;
    
        for (Livro livro : this.livros) {
            int paginasLidas = livro.getPagLidas().size();
            if (paginasLidas > maxPaginasLidas || 
                (paginasLidas == maxPaginasLidas && livro.getNome().compareTo(livroMaisLido.getNome()) > 0)) {
                livroMaisLido = livro;
                maxPaginasLidas = paginasLidas;
            }
        }
    
        return livroMaisLido;
    }

}