import java.util.Map;
import java.util.List;
import java.util.Collection;
import java.time.LocalDate;
import java.util.ArrayList;
import java.io.FileOutputStream;
import java.io.FilterOutputStream;
import java.io.ObjectOutputStream;

public class GestaoRelaxadaTrabalhos extends GestaoTrabalhos {
    private Map<String,List<Entrega>> entregasAtrasadas;
    
    public GestaoRelaxadaTrabalhos(Collection<Aluno> alunos, Map<String,String> alunosGrupo, LocalDate dataLimite){
        super(alunos, alunosGrupo, dataLimite);
    }
    
    public void adicionaEntrega(String codGrupo, Entrega e){
        if (entregasAtrasadas.containsKey(codGrupo)) {
            this.entregasAtrasadas.get(codGrupo).add(e);
        } else {
             List <Entrega> en = new ArrayList<Entrega>();
             en.add(e);
             this.entregasAtrasadas.put(codGrupo, en);
        }
        /*
        FileOutputStream f = new FileOutputStream(codGrupo + e.getDataLimite());
        ObjectOutputStream os = new ObjectOutputStream(f);
        
        os.writeObject(e);
        os.flush();
        os.close();
        */
        
    
        
        
    }
}