package guiao3;

public class BankTest {



    
    public void run() {
        for (int i = 0; i < OPS_PER_WORKER; i++) {
            int r = random.nextInt(100);
            if (r <= 80) {
                transfer();
            } else {
                if (activeIds.size() < INITIAL_ACCOUNTS) {
                    create();
                } else {
                    close();
                }
            }
        }
    }
}
