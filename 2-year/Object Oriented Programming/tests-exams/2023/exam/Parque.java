import java.io.Serializable;

public class Parque implements Comparable<Parque>, Serializable {
    private String codParque;
    private String designacaoParque;
    private int lotacaoMax;
    private int ocupacao;
    private double precoPorHora;
    
    public double precoEstacionamento(Registo r) {
        return 0.0;
    } // calcula o valor a pagar por um estacionamento
    
    public int compareTo(Parque p){
        return 0;
    }
    public String getCodParque(){
        return this.codParque;
    }
    public int  getOcupacao(){
        return this.ocupacao;
    }
    public int getLotacaoMax(){
        return this.lotacaoMax;
    }
    public void setOcupacao(int ocupacao){
        this.ocupacao= ocupacao;
    }
    
    public Parque(String codParque, String designacaoParque, int lotacao, double precoPorHora){
        this.codParque = codParque;
        this.designacaoParque= designacaoParque;
        this.lotacaoMax = lotacao;
        this.precoPorHora = precoPorHora;
        this.ocupacao=0; 
        
    }

}