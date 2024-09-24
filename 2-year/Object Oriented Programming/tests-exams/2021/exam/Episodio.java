import java.time.LocalDateTime;
import java.util.List;

public class Episodio {
    private String nome;
    private double duracao;
    private int classificacao; //dada pelos seus ouvintes (valor de 0..100)
    private List<String> conteudo; //corresponde ao texto que e’ dito
    //quando se reproduz o episodio
    private int numeroVezesTocada; //qts vezes e’ que o podcast foi ouvido
    private LocalDateTime ultimaVez; //regista quando foi reproduzido
    //ultima vez
    
    public Episodio(String nome, double duracao, int classificacao, List<String> conteudo) {
        this.nome = nome;
        this.duracao = duracao;
        this.classificacao = classificacao;
        this.conteudo = conteudo;
        this.numeroVezesTocada = 0;
        this.ultimaVez = null;
    }

    public String getNome() {
        return this.nome;
    }

    public double getDuracao() {
        return this.duracao;
    }

    public int getClassificacao() {
        return this.classificacao;
    }

    public List<String> getConteudo() {
        return this.conteudo;
    }

    public int getNumeroVezesTocada() {
        return this.numeroVezesTocada;
    }

    public LocalDateTime getUltimaVez() {
        return this.ultimaVez;
    }
 




}