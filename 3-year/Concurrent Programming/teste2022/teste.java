import java.util.List;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class teste {
    interface MatchMaker {
        BoundedBuffer waitForConsumer();
        BoundedBuffer waitForProducer();
    }

    // we did this
    class MatchMakerr {
        List<WaitRecord> lp;
        List<WaitRecord> lc;
        Lock lock = new ReentrantLock();

        class WaitRecord {
            BoundedBuffer b = new BoundedBuffer();
            Condition cond = lock.newCondition();
            boolean done = false;
        }

        BoundedBuffer waitForConsumer() {
            lock.lock();
            
            try {
                if (lc.isEmpty()) {
                    WaitRecord w = new WaitRecord();
                    lp.add(w);
                    while (w.done == false) { 
                        w.cond.await();
                    }
                    return w.b;
                } else {
                    WaitRecord w =  lc.remove(0);
                    w.done = true;
                    w.cond.signal();
                    return w.b;
                }
            } finally {
                lock.unlock();
            }
        }

        // waitForProducer is similar to waitForConsumer
    }
}