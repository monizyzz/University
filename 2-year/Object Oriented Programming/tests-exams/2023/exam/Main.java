import java.util.*;

/**
 * Escreva uma descrição da classe Main aqui.
 * 
 * @author (seu nome) 
 * @version (um número da versão ou uma data)
 */
public class Main
{
    // variáveis de instância - substitua o exemplo abaixo pelo seu próprio
    public static void main(String args[])  //static method  
    {
        Parque p1 = new Parque("p1", "a", 3, 5.2);
        Parque p2= new Parque("p2", "b", 45, 2);  
        Parque p3 = new Parque("p3", "c", 32, 24);

        List<Parque> parques = new ArrayList<Parque>();
        parques.add(p1);
        parques.add(p2);
        parques.add(p3);



        Viatura v1 = new Viatura("aa-bb-21", "ze", "carro");
        Viatura v2 = new Viatura("vv-cc-dd", "maria", "mota");
        Viatura v3 = new Viatura("dd-ff-gg", "manel", "carro");
        
        List<Viatura> viaturas = new ArrayList<Viatura>();
        viaturas.add(v1);
        viaturas.add(v2);
        viaturas.add(v3);
        
        GesParques g1 = new GesParques(parques, viaturas);
        
        try
        {
            g1.registaEntrada("p1","vv-cc-dd");
                        
            Thread.sleep(2000);

            g1.registaEntrada("p1","dd-ff-gg");
            
            Thread.sleep(2000);

            g1.registaEntrada("p2","aa-bb-21");
        }
        catch (Exception e)
        {
            e.printStackTrace();
        }
        
        g1.imprimeParque();
        Viatura v = g1.maiorTempoEstacionamento();
        
        //System.out.println(v.getMatricula());
        
        try
        {
            Thread.sleep(2000);
        }
        catch (InterruptedException ie)
        {
            ie.printStackTrace();
        }
        g1.sairDoParque("aa-bb-21");
        g1.imprimeParque();


    }  
}
