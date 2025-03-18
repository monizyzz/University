package guiao3;

import java.util.*;
import java.util.concurrent.locks.ReentrantLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

class Bank {

    private static class Account {
        private int balance;
        private ReentrantLock lock = new ReentrantLock();
        Account(int balance) { this.balance = balance; }
        int balance() { return balance; }
        boolean deposit(int value) {
            balance += value;
            return true;
        }
        boolean withdraw(int value) {
            if (value > balance)
                return false;
            balance -= value;
            return true;
        }
    }

    private Map<Integer, Account> map = new HashMap<Integer, Account>();
    private int nextId = 0;
    ReentrantReadWriteLock lock = new ReentrantReadWriteLock();

    // create account and return account id
    public int createAccount(int balance) {
        Account c = new Account(balance);
        lock.writeLock().lock();
        try {
            int id = nextId;
            nextId += 1;
            map.put(id, c);
            return id;
        } finally {
            lock.writeLock().unlock();
        }
    }

    // close account and return balance, or 0 if no such account
    public int closeAccount(int id) {
        Account c;
        lock.writeLock().lock();
        try {
            c = map.remove(id);
            if (c == null)
                return 0;
            c.lock.lock();
        } finally {
            lock.writeLock().unlock();
        }

        try {
            return c.balance();
        } finally {
            c.lock.unlock();
        }
    }

    // deposit; fails if no such account
    public boolean deposit(int id, int value) {
        Account c;
        lock.readLock().lock();
        try {
            c = map.get(id);
            if (c == null)
                return false;
            c.lock.lock();
        } finally {
            lock.readLock().unlock();
        }

        try {
            return c.deposit(value);
        } finally {
            c.lock.unlock();
        }
    }

    // transfer value between accounts;
    // fails if either account does not exist or insufficient balance
    public boolean transfer(int from, int to, int value) {
        Account cfrom, cto;
        lock.readLock().lock();
        try {
            cfrom = map.get(from);
            cto = map.get(to);
            if (cfrom == null || cto == null)
                return false;

            if (from < to) {
                cfrom.lock.lock();
                cto.lock.lock();
            } else {
                cto.lock.lock();
                cfrom.lock.lock();
            }
        } finally {
            lock.readLock().unlock();
        }

        try {
            return cfrom.withdraw(value) && cto.deposit(value);
        } finally {
            cfrom.lock.unlock();
            cto.lock.unlock();
        }
    }

    // sum of balances in set of accounts; 0 if some does not exist
    public int totalBalance(int[] ids) {
        List<Account> accounts = new ArrayList<>();
        Arrays.sort(ids);

        lock.readLock().lock();
        try {
            for (int i : ids) {
                Account c = map.get(i);
                if (c == null)
                    return 0;
                accounts.add(c);
            }

            for (Account account : accounts) {
                account.lock.lock();
            }
        } finally {
            lock.readLock().unlock();
        }

        try {
            int total = 0;
            for (Account account : accounts) {
                total += account.balance();
            }
            return total;
        } finally {
            for (Account account : accounts) {
                account.lock.unlock();
            }
        }
    }
}
