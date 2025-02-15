package guiao1;

import java.util.ArrayList;
import java.util.List;

public class Ex1 {

    static int N = 10;
    static int I = 5;

    static class Worker implements Runnable {

        @Override
        public void run() {

            System.out.println(Thread.currentThread().getName());
            for (int i = 1; i < I; i++) {
                System.out.println(i);
            }
        }
    }

    public static void main(String[] args) throws InterruptedException {

        List<Thread> list = new ArrayList<>();

        for (int i = 0; i < N; i++) {
            Thread t = new Thread(new Worker());
            t.start();
            list.add(t);
        }

        for (Thread t : list) {
            t.join();
        }

        System.out.println("Main thread finished");
    }
}