import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Scanner;

public class epsilon {


    static void count(ArrayList<String> arl, boolean[] eps) {
        boolean check = true;
        while (check) {
            check = false;
            for (int i = 0; i < arl.size(); i++) {
                boolean res = arl.get(i).length() >= 5 && !eps[(int)arl.get(i).charAt(0)];
                for (int j = 5; j<arl.get(i).length() && !eps[(int)arl.get(i).charAt(0)]; j++) {
                    res = res & eps[(int)arl.get(i).charAt(j)];
                    if (!res)
                        break;
                }
                if (res)
                    eps[(int)arl.get(i).charAt(0)] = true;
                check = check | res;
            }
        }
    }
    public static void main(String args[]) throws IOException {
        BufferedReader input = new BufferedReader(
                new InputStreamReader(new FileInputStream("epsilon.in"), StandardCharsets.UTF_8));
        PrintWriter output = new PrintWriter(new BufferedWriter(
                new OutputStreamWriter(new FileOutputStream("epsilon.out"), StandardCharsets.UTF_8)));
        String str=input.readLine();
        String[] strx = str.split(" ");
        char begin=strx[1].charAt(0);
        int n=Integer.parseInt(strx[0]);
        ArrayList<String> arl=new ArrayList<>(n);
        boolean []eps=new boolean[161];
        for(int i=0;i<n;i++){
            String k="";
            arl.add(k);
            String s=input.readLine();
            arl.add(s);
            if(s.length()==4){
                eps[(int)s.charAt(0)]=true;
            }

        }
        input.close();
        count(arl,eps);
        for(int i=45;i<92;i++){
            if(eps[i]){

            output.print((char)i);
            output.print(" ");
            }}
        output.close();

    }




}

