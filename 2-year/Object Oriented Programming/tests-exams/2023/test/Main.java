import java.util.*;
import java.io.ObjectOutputStream;
import java.io.FileOutputStream;


public class Main
{
    // variáveis de instância - substitua o exemplo abaixo pelo seu próprio
    public static void main(String args[])  //static method  
    {
       ObjectOutputStream oss= null;
        try
        {        
            FileOutputStream os = new FileOutputStream("catarina.txt");
            oss = new ObjectOutputStream(os);
             oss.writeObject("teste");
        }  catch (java.io.IOException ioe)
        {
            ioe.printStackTrace();
        }
        
       
        try
        {
            oss.flush();
        }
        catch (java.io.IOException ioe)
        {
            ioe.printStackTrace();
        }
        try
        {
            oss.close();
        }
        catch (java.io.IOException ioe)
        {
            ioe.printStackTrace();
        }


    }  
}
