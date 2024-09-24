import java.util.List;
import java.util.Map;

import javax.swing.SizeRequirements;

import java.util.ArrayList;
import java.util.HashMap;
import java.time.*;

public class GesParques {
    private List<Parque> parques;
    private List<Viatura> viaturas;
    private List<Registo> registos;
    private Map<String, List<Viatura>> parqueViaturas;

    public GesParques(List <Parque> parques, List <Viatura> viaturas) {
        this.parques = parques;
        this.viaturas = viaturas;
        this.registos = new ArrayList<Registo>();
        this.parqueViaturas = new HashMap<String,List<Viatura>>();
    }

    // questao 6
    public void registaEntrada(String codParque, String matricula) throws Exception {
        Viatura viatura = null;
        Parque parque = null;

        for (Viatura v: this.viaturas) {
            if (v.getMatricula().equals(matricula)) {
                viatura = v;
                break;
            }
        } 

        if (viatura == null) {
            throw new Exception("viatura nao encontrada");
        }

        for (Parque p: this.parques) {
            if (p.getCodParque().equals(codParque)) {
                parque = p;
                if (p.getOcupacao() >= p.getLotacaoMax()) {
                    throw new Exception("parque esta lotado");
                }
                break;
            }
        }

        if (parque == null) {
            throw new Exception("parque nao encontrado");
        }

        for (String codP: this.parqueViaturas.keySet()) {
            if (codP.equals(codParque)) {
                this.parqueViaturas.get(codP).add(viatura);
                parque.setOcupacao(parque.getOcupacao()+1);
                Registo rn = new Registo(parque, viatura);
                this.registos.add(rn);
                return;
            } 
        }

        List<Viatura> novo = new ArrayList<>();
        novo.add(viatura);
        this.parqueViaturas.put(matricula, novo);
        parque.setOcupacao(parque.getOcupacao()+1);


        Registo novoReg = new Registo(parque, viatura);
        this.registos.add(novoReg);
    }

    // questao 7
    public Viatura maiorTempoEstacionamento() {
        Viatura viatura = null;
        double maiorTempo = 0.0;

        for (Registo r: this.registos) {
            double tempo = r.tempoNoParque();
            if (tempo >= maiorTempo) {
                maiorTempo = tempo;
                viatura = r.getViatura();
            }
        }
        return viatura;
    }

    // questao 8 
    public void sairDoParque(String matricula) throws Exception {
        boolean viaturaEncontrada = false;

        for (String codP: this.parqueViaturas.keySet()) {
            List<Viatura> l = this.parqueViaturas.get(codP);
            for (Viatura v: l) {
                if (v.getMatricula().equals(matricula)) {
                    viaturaEncontrada = true;

                    for (Parque p: this.parques) {
                        if (p.getCodParque().equals(codP)) {
                            p.setOcupacao(p.getOcupacao()-1);
                        }
                    }
                    
                    for (Registo r: this.registos) {
                        if (r.getViatura().getMatricula().equals(matricula)) {
                            r.setHoraSaida(LocalDateTime.now());
                        }
                    }
                    
                    l.remove(v);
                    break;
                }
            }
        }

        if (!viaturaEncontrada) {
            throw new Exception("Viatura nao encontrada.");
        }
    }

    // questao 9
    public boolean apenasNumParque(String matricula) {
        int count = 0;

        for (String codP: this.parqueViaturas.keySet()) {
            List<Viatura> l = this.parqueViaturas.get(codP);
            
            for (Viatura v: l) {
                if (v.getMatricula().equals(matricula)) {
                    count++;
                }
            } 
        }

        if (count > 1) {
            return false;
        }

        return true;
    }
}
