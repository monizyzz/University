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
                
        
            case 9:
                Encomenda enc = new Encomenda("Supermercado do Moniz", 123456789, "Rua sem nome", 47, LocalDateTime.now(), new LinhaEncomenda[0]);
                LinhaEncomenda l1 = new LinhaEncomenda("1234", "Chipicao", 0.99, 300, 13, 5);
                LinhaEncomenda l2 = new LinhaEncomenda("2341", "Bollycao", 0.99, 300, 13, 5);
                LinhaEncomenda l3 = new LinhaEncomenda("3412", "Iogurte Grego Pack-4", 0.99, 100, 23, 5);
                LinhaEncomenda l4 = new LinhaEncomenda("4123", "Massa Milaneza", 1.25, 200, 23, 10);
                enc.adicionaLinha(l1);
                enc.adicionaLinha(l2);
                enc.adicionaLinha(l3);
                enc.adicionaLinha(l4);

                out.println("Valor total: " + enc.calculaValorTotal());
                out.println("Valor desconto: " + enc.calculaValorDesconto());
                out.println("Número total de produtos: " + enc.numeroTotalProdutos());
                out.println("Existe encomenda para o código 2341: " + enc.existeProdutoEncomenda("2341"));
                out.println("Existe encomenda para o código 2349: " + enc.existeProdutoEncomenda("2349"));

                out.println("------------ Produtos --------------");
                for(LinhaEncomenda l: enc.getLinhasEncomenda()){
                    out.println("Produto: " + l.getDescricao());
                }

                enc.removeProduto("2341");
                enc.removeProduto("1234");
                enc.removeProduto("3412");
                enc.removeProduto("4123");
                out.println("------------ Produtos --------------");
                for(LinhaEncomenda l: enc.getLinhasEncomenda()){
                    out.println("Produto: " + l.getDescricao());
                }
                break;
        }


        scan.close();
    }


}