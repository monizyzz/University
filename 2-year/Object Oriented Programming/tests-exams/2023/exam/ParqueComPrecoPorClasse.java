import java.util.Map;

public class ParqueComPrecoPorClasse extends Parque {
    private Map<String, Double> precoTipo;

    public ParqueComPrecoPorClasse(String codParque, String designacaoParque, int lotacao, double precoPorHora, Map<String, Double> precoTipo) {
        super(codParque, designacaoParque, lotacao,precoPorHora);
        this.precoTipo = precoTipo;
    }

    public double precoEstacionamento(Registo r) {
        double tempo = r.tempoNoParque();

        double preco = precoTipo.get(r.getViatura().getTipoViatura()); 

        return tempo*preco;
    }

}
