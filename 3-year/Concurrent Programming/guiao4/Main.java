import java.util.ArrayList;
import java.util.List;

public class Main {

    public static class Agreement {
        private int n;

        public Agreement(int n) {
            this.n = n;
        }

        int propose(int choice) throws InterruptedException {

            return choice;
        }
    }

    public static class Barrier {
        private int n;
        private int count = 0;
        private int currRound = 1;

        public Barrier(int n) {
            this.n = n;
        }

        synchronized void await() throws InterruptedException {
            int myRound = currRound;
            count++;

            if (count == n) {
                notifyAll();
                count = 0;
                currRound++;
            } else {
                while (myRound == currRound) {
                    wait();
                }
            }
        }
    }

    public static class Worker implements Runnable {
        private Barrier barrier;

        public Worker(Barrier barrier) {
            this.barrier = barrier;
        }

        @Override
        public void run() {
            for (int i = 0; i < 3; i++) {     
                    try {
                    System.out.println(Thread.currentThread().getName() + " await");
                    barrier.await();
                    System.out.println(Thread.currentThread().getName() + " done");

                } catch (InterruptedException e) {
                    
                    throw new RuntimeException(e);
                }

                try {
                    Thread.sleep(1000);
                } catch (InterruptedException e) {
                    throw new RuntimeException(e);
                }
            }
        }
    }

    public static void main(String[] args) {
        int N = 5;
        Barrier barrier = new Barrier(N);
        List<Thread> threads = new ArrayList<>();

        for (int i = 0; i < N; i++) {
            threads.add(new Thread(new Worker(barrier)));
        }
        
        for (Thread thread : threads) {
            thread.start();
        }

        for (Thread thread : threads) {
            try {
                thread.join();
            } catch (InterruptedException e) {
                throw new RuntimeException(e);
            }
        }

    }

}