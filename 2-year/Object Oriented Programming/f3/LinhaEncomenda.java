import java.lang.StringBuilder;

public class LinhaEncomenda {
    private String referencia;
    private String descricao;
    private double precoAntes;
    private int quantidade;
    private double imposto;
    private double desconto;

    public LinhaEncomenda() {
        this.setRefencia("");
        this.setDescricao("");
        this.setprecoAntes(0);
        this.setQuantidade(0);
        this.setImposto(0.23);
        this.setDesconto(0);
    }

    public LinhaEncomenda(String ref, String desc, double precoAntes, int quantidade, double imposto, double desconto) {
        this.setRefencia(ref);
        this.setDescricao(desc);
        this.setprecoAntes(precoAntes);
        this.setQuantidade(quantidade);
        this.setImposto(imposto);
        this.setDesconto(desconto);
    }

    public LinhaEncomenda(LinhaEncomenda le) {
        this.setRefencia(le.getReferencia());
        this.setDescricao(le.getDescricao());
        this.setprecoAntes(le.getprecoAntes());
        this.setQuantidade(le.getQuantidade());
        this.setImposto(le.getImposto());
        this.setDesconto(le.getDesconto());
    }

    public boolean equals(Object linEnc) {
        if (this == linEnc) 
            return true;

        if (linEnc == null || this.getClass() != linEnc.getClass()) 
            return false;

        LinhaEncomenda le = (LinhaEncomenda) linEnc;

        return (this.getReferencia().equals(le.getReferencia()) &&
                this.getDescricao().equals(le.getDescricao()) &&
                this.getprecoAntes() == le.getprecoAntes() &&
                this.getQuantidade() == le.getQuantidade() &&
                this.getImposto() == le.getImposto() &&
                this.getDesconto() == le.getDesconto()
        );
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Referencia: ").append(this.getReferencia());
        sb.append("\nDescricao: ").append(this.getDescricao());
        sb.append("\nPreço Antes de imposto: ").append(this.getprecoAntes());
        sb.append("\nQuantidade: ").append(this.getQuantidade());
        sb.append("\nImposto: ").append(this.getImposto());
        sb.append("\nDesconto: ").append(this.getDesconto());

        return sb.toString();
    }
    
    public LinhaEncomenda clone() {
        return new LinhaEncomenda(this);
    }

    // 8. (a)
    public String getReferencia() {
        return this.referencia;
    }

    public String getDescricao() {
        return this.descricao;
    }

    public double getprecoAntes() {
        return this.precoAntes;
    }

    public int getQuantidade() {
        return this.quantidade;
    }

    public double getImposto() {
        return this.imposto;
    }

    public double getDesconto() {
        return this.desconto;
    }


    public void setRefencia(String ref) {
        this.referencia = ref;
    }

    public void setDescricao(String desc) {
        this.descricao = desc;
    }

    public void setprecoAntes(double precoAntes) {
        this.precoAntes = precoAntes;
    }

    public void setQuantidade(int quantidade) {
        this.quantidade = quantidade;
    }

    public void setImposto(double imposto) {
        this.imposto = imposto;
    }

    public void setDesconto(double desconto) {
        this.desconto = desconto;
    }

    // 8. (b)
    public double calculaValorLinhaEnc() {
        double preco = this.getprecoAntes() * (1 + this.getImposto());
        double desconto = this.getprecoAntes() * this.getDesconto();

        return(this.getQuantidade() * (preco - desconto));
    }

    // 8. (c)
    public double calculaValorDesconto() {
        double precoSemDesconto = this.calculaValorLinhaEnc();
        double precoComDesconto = this.getprecoAntes() * this.getDesconto();

        return(precoSemDesconto - precoComDesconto);
    }
}