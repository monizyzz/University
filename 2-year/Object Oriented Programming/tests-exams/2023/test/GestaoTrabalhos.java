import java.util.List;
import java.util.Map;
import java.time.*;
import java.util.Collection;
import java.util.ArrayList;

public class GestaoTrabalhos {
    private List<Aluno> alunos;
    private Map<String, List<Aluno>> grupoAlunos; 
    private List<Grupo> grupos;
    private LocalDate dataLimite;

    // questao 6
    public void adicionaAluno(Aluno a) throws Exception {

        for (Aluno r: this.alunos) {
            if (r.getCodAluno().equals(a.getCodAluno())) {
                throw new Exception("ja existe");
            }
        }

        this.alunos.add(a);

    }

    // questao 7
    public Entrega melhorEntrega() {
        int melhorNota = 0;
        String melhorGrupo = "";
        Entrega melhor = new Entrega();

        for (Grupo g: this.grupos) {
            List<Entrega> entregas = g.getEntregas();
            for (Entrega e: entregas) {
                int nota = g.calculaNotaGrupo();
                if (nota > melhorNota) {
                    melhorNota = nota;
                    melhorGrupo = g.getCodGrupo();
                    melhor = e;
                } else if (nota == melhorNota) {
                    if (g.getCodGrupo().compareTo(melhorGrupo) < 0) {
                        melhorNota = nota;
                        melhorGrupo = g.getCodGrupo();
                        melhor = e;
                    }
                }
            }

        }
        return melhor;
    }

    // questao 8
    public void adicionaEntrega(String codGrupo, Entrega e) throws Exception {
        for (String cod: this.grupoAlunos.keySet()) {
            if (codGrupo.equals(cod)) {
                List<Aluno> l = this.grupoAlunos.get(cod);
                for (Aluno a: l) {
                    if (a.getCodAluno().equals(e.getAluno().getCodAluno())) {
                        throw new Exception("avaliador esta no grupo");
                    }
                }
            }
        }

        if (!LocalDate.now().isBefore(e.getDataLimite())) {
            throw new Exception("data ultrapassada");
        } else {
            for (Grupo g: this.grupos) {
                if (g.getCodGrupo().equals(codGrupo)) {
                    //g.addEntrega(e);
                    g.getEntregas().add(e); 
                }
            }
        }
    }

    // questao 9
    public Aluno nome(String cod){
        Aluno a = null;
        for(Aluno r : this.alunos){
            if(r.getCodAluno().equals(cod)){
                a=r;
            }
        }
        return a;
    }

    public GestaoTrabalhos(Collection<Aluno> alunos, Map<String,String> alunosGrupo, LocalDate dataLimite){
        this.alunos = new ArrayList<Aluno>();
        for(Aluno a: alunos){
            this.alunos.add(a);
        }
        
        for (String codAluno: alunosGrupo.keySet()) {
            String grupo = alunosGrupo.get(codAluno);
            if(this.grupoAlunos.containsKey(grupo)){
                this.grupoAlunos.get(grupo).add(nome(codAluno));
            } else {
                List<Aluno> novo = new ArrayList<Aluno>();
                novo.add(nome(codAluno));
                this.grupoAlunos.put(grupo, novo);

            }
        }
        this.dataLimite = dataLimite;
    }

    

}
