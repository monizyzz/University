import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;

public class teste {
    public interface Jogo {
        Partida participa();
    }

    public interface Partida {
        String adivinha(int n);
    }

    Lock lock = new ReentrantLock();
    Condition cond = ;
    int numPlayers = 0;
    Partida curr = ;
    

    Partida participa() {
        lock.lock();
        try {
            numPlayers++;
            Partida myPartida = curr;
            if (numPlayers < 4) {
                while (myPartida == curr) {
                    cond.await();
                }
            } else {
                cond.signalAll();
                numPlayers = 0;
                curr = new Partida();
            }

            return myPartida;
        }
        finally {
            lock.unlock();
        }
    }

    public class Partida {
        int tries = 100;
        int num = 23; // num aleatorio entre 1 e 100

        long start = System.currentTimeMillis();

        Lock lock = new ReentrantLock(); 

        bool won = false;


        public String adivinha(int n) {
            lock.lock();
            try {
                if (won) {
                    return "PERDEU";
                }
                if (tries == 0) {
                    return "PERDEU";
                }
                if (System.currentTimeMillis() - start > 60000) {
                    return "PERDEU";
                }

                if (n == num) {
                    won = true;
                    return "GANHOU";
                } else if (n < num) {
                    tries--;
                    return "MAIOR";
                } else {
                    tries--;
                    return "MENOR";
                }

            } finally {
                lock.unlock();
            }
        }
    }
}