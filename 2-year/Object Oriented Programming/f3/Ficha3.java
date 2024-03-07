import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
import java.util.Scanner;
import static java.lang.System.out;

public class Ficha3 {

    public static void main(String[] args){
        Scanner scan = new Scanner(System.in);
        out.print("Exercício: ");
        int ex = scan.nextInt();
        out.print("Alínea: ");
        String alinea1 = scan.next();
        out.println();

        switch(ex) {
            case 1:
                Circulo circulo = new Circulo(1, 1, 9);

                switch (alinea1) {
                    case "a":
                        double x = circulo.getX();
                        out.println("Valor em x: " + x);
                        break;
                        
                    case "b":
                        double y = circulo.getY();
                        out.println("Valor em y: " + y);
                        break;

                    case "c":
                        double raio = circulo.getRaio();
                        out.println("Valor do raio: " + raio);
                        break;

                    case "d":
                        circulo.setX(2);
                        circulo.setY(2);
                        circulo.setRaio(16);
                        out.println("X: " + circulo.getX());
                        out.println("Y: " + circulo.getY());
                        out.println("Raio: " + circulo.getRaio());
                        break;

                    case "e":
                        circulo.alteraCentro(10, 10);
                        out.println("X: " + circulo.getX());
                        out.println("Y: " + circulo.getY());
                        break;

                    case "f":
                        out.println("Área: " + circulo.calculaArea());
                        break;

                    case "g":
                        out.println("Perímetro: " + circulo.calculaPerimetro());
                        break;

                    default:
                        out.println("Não existe essa alínea");
                        break;
                }
                break;
            
            case 5:
                Lampada lamp = new Lampada(); // desligada
                
                // ON
                LocalDateTime inicioOn = LocalDateTime.now();
                lamp.lampON();
                out.println("Estado: " + lamp.getEstado());
                LocalDateTime fimOn = LocalDateTime.now();
                
                long timeOn = ChronoUnit.MILLIS.between(inicioOn, fimOn);
                
                // ECO
                LocalDateTime inicioEco = LocalDateTime.now();
                lamp.lampECO();
                out.println("Estado: " + lamp.getEstado());
                LocalDateTime fimEco = LocalDateTime.now();
                
                long timeEco = ChronoUnit.MILLIS.between(inicioEco, fimEco);
                
                // OFF
                lamp.lampOFF();
                out.println("Estado: " + lamp.getEstado());
                
                long consumo = timeOn * 10 + timeEco * 5;
                out.println(lamp.totalConsumo() + " == " + consumo);
                out.println(lamp.periodoConsumo());
                break;
                
        }



        scan.close();
    }


}