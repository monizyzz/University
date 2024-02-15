import java.util.Scanner;

public class Input_output {
    
    public static void main(String[] args){
        int idade;
        Scanner id = new Scanner(System.in);

        System.out.print("Indique a sua idade: ");
        idade = id.nextInt();

        System.out.println("A idade é " + idade);
        System.out.print("A idade é "); System.out.println(idade);
        System.out.printf("A idade é %d\n", idade);

        id.close();
    }
}