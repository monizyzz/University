public class Lampada {

    public enum Modo {
        ON,
        OFF,
        ECO
    }
    
    // variáveis de instância
    private Modo modo;
    private double cpsON; // consumo por segundo ON
    private double cpsECO; // consumo por segundo ECO
    private double consumoTotal;
    private double tempoDeConsumo;
    
    public Lampada() {
        this.modo = Modo.OFF;
        this.cpsON = 2;
        this.cpsECO = 1;
        this.consumoTotal = 0;
        this.tempoDeConsumo = 0;
    }

    public Lampada(Modo x, double cpsOn, double cpsEco, double ct, double tc) {
        this.modo = x;
        this.cpsON = cpsOn;
        this.cpsECO = cpsEco;
        this.consumoTotal = ct;
        this.tempoDeConsumo = tc;
    }
    
    public Lampada(Lampada lamp) {
        this.modo = lamp.getEstado();
        this.cpsON = lamp.getcpsON();
        this.cpsECO = lamp.getcpsECO();
        this.consumoTotal = lamp.getconsumoTotal();
        this.tempoDeConsumo = lamp.gettempoDeConsumo();
    }

    public Lampada clone() {
        return new Lampada(this);    
    }

    public boolean equals(Object o) {
        if (this == o) return true;
        
        if (o == null || this.getClass() != o.getClass()) return false;
    
        Lampada l = (Lampada) o;

        return( this.getEstado() == l.getEstado() &&
                this.getcpsON() == l.getcpsON() &&
                this.getcpsECO() == l.getcpsECO() && 
                this.getconsumoTotal() == l.getconsumoTotal() &&
                this.gettempoDeConsumo() == l.gettempoDeConsumo()
            );
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Lâmpada: ").append(this.getEstado()).append("\n");
        sb.append("Consumo por segundo em ON: ").append(this.getcpsON()).append("\n");
        sb.append("Consumo por segundo em ECO: ").append(this.getcpsECO()).append("\n");
        sb.append("Consumo total: ").append(this.getconsumoTotal()).append("\n");
        sb.append("Tempo de consumo: ").append(this.gettempoDeConsumo()).append("\n");

        return sb.toString();
    }



    public Modo getEstado() {
        return this.modo;
    }

    public void setEstado(Modo x) {
        this.modo = x;
    }


    public double getcpsON() {
        return this.cpsON;
    }

    public void setcpsON(double x) {
        this.cpsON = x;
    }


    public double getcpsECO() {
        return this.cpsECO;
    }

    public void setcpsECO(double x) {
        this.cpsECO = x;
    }


    public double getconsumoTotal() {
        return this.consumoTotal;
    }

    public void setconsumoTotal(double x) {
        this.consumoTotal = x;
    }


    public double gettempoDeConsumo() {
        return this.tempoDeConsumo;
    }

    public void settempoDeConsumo(double x) {
        this.tempoDeConsumo = x;
    }

    private void calculaON() {
        this.consumoTotal += (System.currentTimeMillis() - tempoDeConsumo) * cpsON;
    }

    private void calculaECO() {
        this.consumoTotal += (System.currentTimeMillis() - tempoDeConsumo) * cpsECO;
    }

    // 5. (a)
    public void lampON() {
        if (this.modo == Modo.ECO) {
            calculaECO();
        } 
        this.tempoDeConsumo = System.currentTimeMillis();       
        this.modo = Modo.ON;
    }

    // 5. (b)
    public void lampOFF() {
        if (this.modo == Modo.ON) {
            calculaON();
        }

        if (this.modo == Modo.ECO) {
            calculaECO();
        }

        this.tempoDeConsumo = System.currentTimeMillis();
        this.modo = Modo.OFF;
    }

    // 5. (c)
    public void lampECO() {
        if (this.modo == Modo.ON) {
            calculaON();
        } 
        this.tempoDeConsumo = System.currentTimeMillis();       
        this.modo = Modo.ECO;
    }

    // 5. (d)
    public double totalConsumo() {
        return periodoConsumo() + consumoTotal;
    }
    
    // 5. (e)
    public double periodoConsumo() {
        double atual = 0;

        if (modo == Modo.ON) {
            atual = (System.currentTimeMillis() - this.tempoDeConsumo) * this.cpsON;
        } else if (modo == Modo.ECO) {
            atual = (System.currentTimeMillis() - this.tempoDeConsumo) * this.cpsECO;
        }

        return atual;
    }
}