import java.io.Serializable;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class HoteisPOO implements Serializable {
    private Map<String, Hotel> hoteis;

    public HoteisPOO(Map<String, Hotel> hoteis) {
        this.hoteis = hoteis;
    }

    // questao 8
    public List<String> hoteisMaisOcupados() {
        return this.hoteis.values().stream()
                    .sorted((h1, h2) -> {
                        int nDiash1 = h1.getRegistos().values().map(Registo::numDiasReserva).sum();

                        int nDiash2 = h2.getRegistos().values().map(Registo::numDiasReserva).sum();

                        if (nDiash1 == nDiash2) {
                            return h2.getNome().compareTo(h1.getNome());
                        }

                        return Integer.compare(nDiash2, nDiash1);
                    })
                    .limit(3)
                    .map(Hotel::getNome)
                    .collect(Collectors.toList());
    }

}