import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;

public class Main {
    public static void main(String[] args) throws IOException {
        // to read from the console
        BufferedReader cli = new BufferedReader(new InputStreamReader(System.in));
        
        Socket socket = new Socket("localhost", 12345);
        BufferedReader in = new BufferedReader(new InputStreamReader(socket.getInputStream()));
        PrintWriter out = new PrintWriter(socket.getOutputStream(), true);

        String line;
        while ((line = cli.readLine()) != null) {
            out.println(line);
            out.flush();


            String sum = in.readLine();
            System.out.println(sum);
        } 

        socket.shutdownOutput();


        String avg = in.readLine();
        System.out.println(avg);

        socket.close();
    }
}