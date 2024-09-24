import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class UberUM {
    private Map<String, Veiculo> veiculos;

    // questao 1
    public UberUM(Set<Map.Entry<String,Veiculo>> info) {
        this.veiculos = new HashMap<>();
        for (Map.Entry<String,Veiculo> entry: info) {
            this.veiculos.put(entry.getKey(), entry.getValue().clone());
        }
    }

    public Map<String,List<Veiculo>> porMarcaPorValor() {
        // marca 

        

        


    }

}
