import java.time.LocalDate;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.HashMap;

public class Hotel {
    private String nome;
    private Map<String, Quarto> quartos; // usei Map pois é de facíl acesso e eficiencia. Para associar String, o número de quarto, ao Quarto 
    private Map<Integer, Registo> registos; // usei Map pois é de facíl acesso e eficiencia. Para associar Integer, o número de registo, ao Registo 

    public String getNome() {
        return this.nome;
    }

    public Map<Integer, Registo> getRegistos() {
        return this.registos;
    }

    // questao 6  
    public Hotel(Iterator<Quarto> quartos) {
        this.registos = new HashMap<>();
        this.quartos = new HashMap<>();

        while (quartos.hasNext()) {
            Quarto quarto = quartos.next();
            this.quartos.put(quarto.getNumeroQuarto(), quarto);
        }
    }

    // questao 7
    public void adicionaRegisto(LocalDate entrada, LocalDate saida, String numQuarto) throws Exception {
        Quarto quarto = null;

        for (Quarto q : this.quartos.values()) {
            if (q.getNumeroQuarto().equals(numQuarto)) {
                quarto = q;
                break;
            }
        }

        if (quarto == null) {
            throw new Exception("Quarto nao existe");
        }

        Registo r = new Registo(this.registos.size()+1, entrada, saida, quarto);
        this.registos.put(r.getnumRegisto(), r);
    }

    // ou
    public void adicionaRegistoV2(LocalDate entrada, LocalDate saida, String numQuarto) throws Exception {

        if (! this.quartos.containsKey(numQuarto)) {
            throw new Exception("Quarto nao existe");
        }

        Quarto quarto = this.quartos.get(numQuarto);

        Registo r = new Registo(this.registos.size()+1, entrada, saida, quarto);
        this.registos.put(r.getnumRegisto(), r);
    }


    public List<Quarto> quartosLivres(LocalDate entrada, LocalDate saida) throws Exception {
        return this.quartos.values().stream()
                    .filter(q -> q.estaLivre(entrada, saida))
                    .collect(Collectors.toList());
    }


}
