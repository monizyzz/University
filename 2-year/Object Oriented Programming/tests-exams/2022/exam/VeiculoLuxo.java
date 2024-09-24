

public class VeiculoLuxo extends Veiculo implements Comparable<VeiculoLuxo> {
    private float taxaLuxo;

    public float custoViagem(float distancia) {
        return distancia * getPrecoKm() * (1.1 + this.taxaLuxo);
    }

}
