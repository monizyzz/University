import java.util.List;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Iterator;

public class HotelComFilaEspera extends Hotel {
    private List<Registo> reservasEmEspera;

    public HotelComFilaEspera(Iterator<Quarto> quartos) {
        super(quartos);
        this.reservasEmEspera = new ArrayList<>();
    }

    // questao 9 
    public void adicionaReserva(LocalDate entrada, LocalDate saida) {
        Registo r = new Registo(this.reservasEmEspera.size()+1, entrada, saida, null);
        this.reservasEmEspera.add(r);
    }    

    public void ocupaQuartoComReservaEmEspera() {
        Iterator<Registo> it = this.reservasEmEspera.iterator();

        while (it.hasNext()) {
            Registo reserva = it.next();
            try {
                Quarto q = quartosLivres(reserva.getDataInicio(), reserva.getDataFim()).get(0);
                reserva.setQuarto(q);
                adicionaRegisto(reserva.getDataInicio(), reserva.getDataFim(), q.getNumeroQuarto());
                it.remove();
                break;
            } catch (Exception e) {
                continue;
            }
        }
    }

    
}
