import java.time.LocalDateTime;
import java.util.Arrays;

public class Encomenda {
    private String nomeCliente;
    private double numFiscal;
    private String morada;
    private double numEnc;
    private LocalDateTime dataEnc;
    private LinhaEncomenda[] linhasEncomenda;

    public Encomenda() {
        this.setNomeCliente("");
        this.setNumFiscal(0);
        this.setMorada("");
        this.setNumEnc(0);
        this.setDataEnc(LocalDateTime.now());
        this.linhasEncomenda = new LinhaEncomenda[0];
    }

    public Encomenda(String nomeCliente, double numFiscal, String morada, double numEnc, LocalDateTime dataEnc, LinhaEncomenda[] linhasEncomenda) {
        this.setNomeCliente(nomeCliente);
        this.setNumFiscal(numFiscal);
        this.setMorada(morada);
        this.setNumEnc(numEnc);
        this.setDataEnc(dataEnc);
        this.linhasEncomenda = new LinhaEncomenda[0];
    }

    public Encomenda(Encomenda e) {
        this.setNomeCliente(e.getNomeCliente());
        this.setNumFiscal(e.getNumFiscal());
        this.setMorada(e.getMorada());
        this.setNumEnc(e.getNumEnc());
        this.setDataEnc(e.getDataEnc());
    }

    public boolean equals(Object o) {
        if (this == o) 
            return true;
        

        if (o == null || this.getClass() != o.getClass())
            return false;
        

        Encomenda e = (Encomenda) o;

        return this.getNomeCliente().equals(e.getNomeCliente()) && this.getNumFiscal() == e.getNumFiscal()
                && this.getMorada().equals(e.getMorada()) && this.getNumEnc() == e.getNumEnc()
                && this.getDataEnc().equals(e.getDataEnc()) && Arrays.equals(this.getLinhasEncomenda(), e.getLinhasEncomenda());
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();

        sb.append("Nome do Cliente: ").append(this.getNomeCliente());
        sb.append("\nNúmero Fiscal: ").append(this.getNumFiscal());
        sb.append("\nMorada: ").append(this.getMorada());
        sb.append("\nNúmero da Encomenda: ").append(this.getNumEnc());
        sb.append("\nData da Encomenda: ").append(this.getDataEnc());
        sb.append("\nLinhas de Encomenda: ").append(Arrays.toString(this.getLinhasEncomenda()));

        return sb.toString();
    }

    public Encomenda clone() {
        return new Encomenda(this);
    }

    // 9. (a)
    public String getNomeCliente() {
        return this.nomeCliente;
    }

    public double getNumFiscal() {
        return this.numFiscal;
    }

    public String getMorada() {
        return this.morada;
    }

    public double getNumEnc() {
        return this.numEnc;
    }

    public LocalDateTime getDataEnc() {
        return this.dataEnc;
    }

    public LinhaEncomenda[] getLinhasEncomenda() {
        LinhaEncomenda[] r = new LinhaEncomenda[this.linhasEncomenda.length];
        System.arraycopy(this.linhasEncomenda, 0, r, 0, this.linhasEncomenda.length);

        return r;
    }

    public void setNomeCliente(String nomeCliente) {
        this.nomeCliente = nomeCliente;
    }

    public void setNumFiscal(double numFiscal) {
        this.numFiscal = numFiscal;
    }

    public void setMorada(String morada) {
        this.morada = morada;
    }

    public void setNumEnc(double numEnc) {
        this.numEnc = numEnc;
    }

    public void setDataEnc(LocalDateTime dataEnc) {
        this.dataEnc = dataEnc;
    }

    public void setLinhasEncomenda(LinhaEncomenda[] le) {
        this.linhasEncomenda = new LinhaEncomenda[le.length];
        System.arraycopy(le, 0, this.linhasEncomenda, 0, le.length);
    }

    // 9. (b)
    public double calculaValorTotal() {
        double total = 0;

        for (LinhaEncomenda le: this.getLinhasEncomenda()) {
            total += le.calculaValorLinhaEnc();
        }

        return total;
    }

    // 9. (c)
    public double calculaValorDesconto() {
        double total = 0;

        for (LinhaEncomenda le: this.getLinhasEncomenda()) {
            total += le.calculaValorDesconto();
        }

        return total;
    }

    // 9. (d)
    public int numeroTotalProdutos() {
        int total = 0;

        for (LinhaEncomenda le: this.getLinhasEncomenda()) {
            total += le.getQuantidade();
        }

        return total;
    }

    // 9. (e)
    public boolean existeProdutoEncomenda(String refProduto) {
        boolean found = false;

        for (int i = 0; i < this.linhasEncomenda.length && !found; i += 1) {
            if (this.linhasEncomenda[i].getReferencia().equals(refProduto)) 
                found = true;
        }

        return found;
    }

    // 9. (f)
    public void adicionaLinha(LinhaEncomenda linha) {
        int n = this.linhasEncomenda.length;
        LinhaEncomenda[] encomenda = this.getLinhasEncomenda();
        LinhaEncomenda[] newEncomenda = new LinhaEncomenda[n + 1];

        System.arraycopy(encomenda, 0, newEncomenda, 0, n);
        newEncomenda[n] = linha;
        this.setLinhasEncomenda(newEncomenda);
    }

    // 9. (g)
    public void removeProduto(String codProd) {

        if(this.linhasEncomenda.length > 0) {

            LinhaEncomenda[] enc = this.getLinhasEncomenda();
            LinhaEncomenda[] novo = new LinhaEncomenda[enc.length - 1];
            
            boolean found = false;
            int index = -1;

            for (int i = 0; i < enc.length && !found; i += 1) {
                if (enc[i].getReferencia().equals(codProd))
                    found = true;
                index = i;
            }

            System.arraycopy(enc, 0, novo, 0, index);
            System.arraycopy(enc, index + 1, novo, index, enc.length - index - 1);
            this.setLinhasEncomenda(novo);
        }
    }
}