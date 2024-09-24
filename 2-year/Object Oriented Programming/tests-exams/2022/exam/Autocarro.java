


public class Autocarro extends Veiculo implements Comparable<Autocarro> {
    private int lotacao; //lotação máxima do autocarro

    public float custoViagem(float distancia) {
    if (this.lotacao > 10)
        return distancia * getPrecoKm() * 0.5/this.lotacao;
    else
        return distancia * 0.75 * this.lotacao;
    }

}