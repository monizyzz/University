package guiao1;

import java.util.ArrayList;
import java.util.List;

public class Ex2v2 {
    static int N = 100;
    static int I = 100;
    static int counter = 0;

    static class Counter {

        int value = 0;

        public synchronized void increment() {
            value++;
        }

        public synchronized int getCount() {
            return value;
        }
    }

    static class Worker implements Runnable {

        private Counter counter;

        Worker(Counter counter) {
            this.counter = counter;
        }
        

        @Override
        public void run() {
            for (int i = 1; i <= I; i++) {
                counter.increment();
            }
            
        }
    }

    public static void main(String[] args) throws InterruptedException {

        List<Thread> list = new ArrayList<>();

        for (int i = 0; i < N; i++) {
            Thread t = new Thread(() -> {
                for (int j = 0; j < I; j++) {
                    counter++;
                }
            });

            t.start();
            list.add(t);
        }

        for (Thread t : list) {
            t.join();
        }

        System.out.println("Counter: " + counter);
        System.out.println("Main thread finished");
    }
}