import java.io.Serializable;
import java.util.Set;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class LivrosDigitais implements Comparable<LivrosDigitais>, Serializable {
    private Set<Utilizador> usersRegistados;

    public Map<String,List<Livro>> livrosPorEditora() {
        Map<String,List<Livro>> edit = new HashMap<>();

        for (Utilizador user: this.usersRegistados) {
            for (Livro l: user.getColecao()) {
                String editora = l.getEditora();
                if ( !edit.containsKey(editora) ) {
                    edit.put(editora, new ArrayList<>());
                }
                edit.get(editora).add(l.clone());
            }
        }

        return edit;
    }
    
}
