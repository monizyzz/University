import java.lang.Math;
import java.lang.StringBuilder;

public class Circulo {
    private double x;
    private double y;
    private double raio;

    public Circulo() {
        this.setX(0);
        this.setY(0);
        this.setRaio(0);
    }

    public Circulo(double x, double y, double raio) {
        this.setX(x);
        this.setY(y);
        this.setRaio(raio);
    }

    public Circulo(Circulo c) {
        this(c.getX(), c.getY(), c.getRaio());
    }

    public boolean equals(Object o) {
        if (this == o) return true;

        if (o == null || this.getClass() != o.getClass()) return false;

        Circulo c = (Circulo) o;

        return( this.getX() == c.getX() &&
                this.getY() == c.getY() &&
                this.getRaio() == c.getRaio()
            );
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Coordenada x: ").append(this.getX()).append("\n");
        sb.append("Coordenada y: ").append(this.getY()).append("\n");
        sb.append("Raio: ").append(this.getRaio()).append("\n");

        return sb.toString();
    }

    public Circulo clone(Circulo circulo){
        return new Circulo(this);
    }

    // 1. (a)
    public double getX() {
        return this.x;
    }
    
    // 1. (b)
    public double getY() {
        return this.y;
    }
    
    // 1. (c)
    public double getRaio() {
        return this.raio;
    }
    
    // 1. (d)
    public void setX(double x) {
        this.x = x;
    }

    public void setY(double y) {
        this.y = y;
    }

    public void setRaio(double raio) {
        this.raio = raio;
    }  

    // 1. (e)
    public void alteraCentro(double x, double y) {
        this.x = x;
        this.y = y;
    }

    // 1. (f)
    public double calculaArea() {
        return Math.PI * Math.pow(raio,2);
    }

    // 1. (g)
    public double calculaPerimetro() {
        return 2 * Math.PI * raio;
    } 
}