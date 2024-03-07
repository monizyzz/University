import static java.lang.System.out;

public class Turma {
    private int[][] notasTurma;
    private int numAlunos;
    private int ucs;

    public Turma(int numAlunos, int ucs) {
        notasTurma = new int[numAlunos][ucs];
        this.numAlunos = numAlunos;
        this.ucs = ucs;
    }

    public void setNota(int aluno, int uc, int nota) {
        this.notasTurma[aluno][uc] = nota;
    }

    public int getNota(int aluno, int uc) {
        return this.notasTurma[aluno][uc];
    }

    // 5. (a)
    public void atualizarPauta(int aluno, int uc, int nota) {
        setNota(aluno, uc, nota);
    }

    // 5. (b)
    public int somaNotasUcs(int ucIndex) {
        int soma = 0;

        for (int uc = 0; uc < this.ucs; uc += 1) {
            soma += getNota(uc, ucIndex);
        }

        return soma;
    }

    // 5. (c)
    public float mediaAluno(int Alunoindex) {
        float media = 0;

        for (int uc = 0; uc < this.ucs; uc += 1) {
            media += getNota(Alunoindex, uc);
        }

        media = media / this.ucs;

        return media;
    }

    // 5. (d)
    public float mediaUc(int ucIndex) {
        
        return (float)somaNotasUcs(ucIndex)/this.ucs;       
    }

    // 5. (e)
    public int notaMaisAlta() {
        int maiorNota = notasTurma[0][0];

        for (int aluno = 0; aluno < this.numAlunos; aluno += 1) {
            for (int uc = 0; uc < this.ucs; uc += 1)
                if (maiorNota < getNota(aluno, uc)) {
                    maiorNota = getNota(aluno, uc);
                }
        }

        return maiorNota;
    }
    
    // 5. (f)
    public int notaMaisBaixa() {
        int menorNota = notasTurma[0][0];

        for (int aluno = 0; aluno < this.numAlunos; aluno += 1) {
            for (int uc = 0; uc < this.ucs; uc += 1)
                if (menorNota > getNota(aluno, uc)) {
                    menorNota = getNota(aluno, uc);
                }
        }

        return menorNota;
    }
    
    // 5. (g)
    public void notasAcimaDe(int valor) {
        int[] notas = new int[this.numAlunos*this.ucs];
        int k = 0;
        out.println("Notas acima deste de " + valor + " : ");

        for (int aluno = 0; aluno < this.numAlunos; aluno += 1) {
            for (int uc = 0; uc < this.ucs; uc += 1)
                if (valor < getNota(aluno, uc)) {
                    notas[k] = getNota(aluno, uc);
                    out.print(notas[k]);
                    out.print("  ");
                    k += 1;
                }
        }
        out.println();
    }
    
    // 5. (h)
    public void notasTodosAlunos() {
        String notas = "";

        for (int aluno = 0; aluno < this.numAlunos; aluno += 1) {
            notas = notas + "-- ALUNO " + aluno + " --\n";
            for (int uc = 0; uc < this.ucs; uc += 1) {
                notas = notas + "uc: " + uc + " -> " + getNota(aluno, uc) + " | ";
            }
            notas += "\n\n";    
        }   
        out.print(notas);
    }
    
    // 5. (i)
    public void mediaUcMaisAlta() {
        float media = 0, average = 0;
        int indice_uc = 0, uc;

        for (uc = 0; uc < this.ucs; uc += 1) {
            average = mediaUc(uc);
            
            if (media < average) {
                media = average;
                indice_uc = uc;
            }
        }
        
        out.println("A uc com índice de maior média é " + indice_uc + " com " + media);
    }
}