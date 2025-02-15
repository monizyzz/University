package guiao1;

import java.util.ArrayList;
import java.util.List;

public class Ex3 {
    static int N = 1000;
    static int I = 1000;

    static class Counter {

        private int value = 0;

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
            for (int i = 0; i < I; i++) {
                counter.increment();
            }
            
        }
    }

    public static void main(String[] args) throws InterruptedException {
        Counter counter = new Counter(); // shared resource

        List<Thread> list = new ArrayList<>();

        for (int i = 0; i < N; i++) {
            Thread t = new Thread(new Worker(counter));

            t.start();
            list.add(t);
        }

        for (Thread t : list) {
            t.join();
        }

        System.out.println("Counter: " + counter.getCount());
        System.out.println("Main thread finished");
    }
}