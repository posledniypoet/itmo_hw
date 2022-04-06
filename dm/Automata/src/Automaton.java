import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.*;

public class Automaton {

    private static boolean check(String str,char begin,ArrayList<ArrayList<ArrayList<Character>>> sost,int z){
        if(z<str.length()-1){
            if(begin=='0'){
                return false;
            }
            if (!sost.get(begin - 'A').get(str.charAt(z) - 'a').isEmpty()) {
                boolean res=false;
                for(int i=0;i<sost.get(begin-'A').get(str.charAt(z)-'a').size();i++){
                    res=res||check(str,sost.get(begin-'A').get(str.charAt(z)-'a').get(i),sost,z+1);
                }
                return res;

            } else return false;
        }
        else{
           boolean otv=false;
           if(sost.get(begin-'A').get(str.charAt(z)-'a').contains('0'))return true;

            return otv;

        }
    }

    public static void main(String args[]) throws IOException {
        BufferedReader input = new BufferedReader(
                new InputStreamReader(new FileInputStream("automaton.in"), StandardCharsets.UTF_8));
        PrintWriter output = new PrintWriter(new BufferedWriter(
                new OutputStreamWriter(new FileOutputStream("automaton.out"), StandardCharsets.UTF_8)));
        String str=input.readLine();
        String[] strx = str.split(" ");
        char begin=strx[1].charAt(0);
        int n=Integer.parseInt(strx[0]);

        ArrayList<ArrayList<ArrayList<Character>>> sost = new ArrayList<ArrayList<ArrayList<Character>>>();
        for(int i=0;i<30;i++){
            ArrayList<ArrayList<Character>> arl1=new ArrayList<ArrayList<Character>>();
            for(int j=0;j<30;j++){
                ArrayList<Character> arl=new ArrayList<Character>();
                arl1.add(arl);
            }
            sost.add(arl1);
        }
        for(int i=0;i<n;i++) {
            String s = input.readLine();
            if (s.length() > 6) {
                sost.get(s.charAt(0) - 'A').get(s.charAt(5) - 'a').add(s.charAt(6));
            } else {
                sost.get(s.charAt(0) - 'A').get(s.charAt(5) - 'a').add('0');
            }
        }
        String mx=input.readLine();
        int m;
        if(mx.length()>1){
        m=Integer.parseInt(mx.substring(0,2));}
        else{
        m=Integer.parseInt(mx.substring(0,1));
        }
        String[] strochka=new String[m];
        for(int i=0;i<m;i++){
            strochka[i]=input.readLine();
        }
        input.close();
        for(int i=0;i<m;i++){
            if(i!=(m-1)){
                if(check(strochka[i],begin,sost,0)){
                    output.print("yes");
                }
                else{
                    output.print("no");
                }
            }
            else{
                if(check(strochka[i],begin,sost,0)){
                    output.print("yes");
                }
                else{
                    output.print("no");
                }

            }
        }

        output.close();

    }
}