import java.time.LocalDate;
import java.time.temporal.ChronoUnit;


public class Datas {
    private LocalDate[] data;
    private int numInsertions;
    private int size;

    public Datas(int size){
        this.numInsertions = 0;
        this.size = size;
        this.data = new LocalDate[size];
    }

    // 2. (a)
    public void insereData(LocalDate data) {
        if (numInsertions < size) {
            this.data[this.numInsertions++] = data;
        }
    }

    // 2. (b)
    public LocalDate dataMaisProxima(LocalDate data) {
        long mindaysBetween = ChronoUnit.DAYS.between(this.data[0], data);
        int index = 0;

        for (int i = 0; i < this.numInsertions; i += 1) {
            long daysBetween = ChronoUnit.DAYS.between(this.data[i], data);
            if (daysBetween < mindaysBetween) {
                index = i;
                mindaysBetween = daysBetween;
            }
        }
        
        return this.data[index];
    }
     
    // 2. (c)
    public String toString() {
        String string = "";
        int i;

        for (i = 0; i < this.numInsertions; i += 1)
            string += this.data[i] + "\n";

        return string;
    }   
}