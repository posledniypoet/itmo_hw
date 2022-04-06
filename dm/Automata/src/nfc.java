import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Scanner;
import java.util.zip.CheckedInputStream;

public class nfc {
    static class Pair {
        boolean x;
        Character y;
        Character z;

        Pair(boolean x, Character y, Character z) {
            this.x = x;
            this.y = y;
            this.z=z;
        }
    }
    static long ans(int v, int l, int r,long dp[][][],ArrayList<ArrayList<Pair>> sost,String word) {
        if (dp[v][l][r] == -1) {
            long res = 0;
            Iterator<Pair> iter = sost.get(v).iterator();
            while (iter.hasNext()) {
                Pair c=iter.next();
                if (c.x) {
                    if (l + 1 == r && word.charAt(l) == c.y) {
                        ++res;
                        res %= 1000000007;
                    }
                } else {
                    for (int i = l + 1; i < r; ++i) {
                        res = (ans(c.y - 'A', l, i,dp,sost,word) * ans(c.z - 'A', i, r,dp,sost,word) + res) % 1000000007;
                    }
                }
            }

            dp[v][l][r] = res;
        }

        return dp[v][l][r];
    }
    public static void main(String args[]) throws IOException {
        Scanner in=new Scanner(System.in);
     long [][][] dp=new long[26][26][26];
        String str=in.nextLine();
        String[] strx = str.split(" ");
        char begin=strx[1].charAt(0);
        int n=Integer.parseInt(strx[0]);
        ArrayList<ArrayList<Pair>> sost = new ArrayList<ArrayList<Pair>>();
        for(int i=0;i<26;i++){
            ArrayList<Pair> arl=new ArrayList<Pair>();
            sost.add(arl);
        }
        String a, b;
        for (int i = 0; i < n; ++i) {
            a=in.nextLine();
            int v = a.charAt(0) - 'A';

            Character toa = a.charAt(5);
            Character tob='c';
            Boolean tol;
            if (a.length() > 6) {
                tob = a.charAt(6);
                tol = false;
            } else {
                tol = true;
            }
            Pair rule=new Pair(tol,toa,tob);
            sost.get(v).add(rule);
        }

        String word=in.nextLine();
        System.out.println(ans(begin - 'A', 0, word.length(),dp,sost,word));
    }
}
