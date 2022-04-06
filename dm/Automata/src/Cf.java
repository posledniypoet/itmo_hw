import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.*;

public class Cf {


    static class Pair {
        Character x;
        String y;

        Pair(Character x, String y) {
            this.x = x;
            this.y = y;
        }
    }



    public static void main(String args []) throws IOException {
        ArrayList<Pair> rules=new ArrayList<Pair>();
        ArrayList<String>[] edges = new ArrayList[30];
        int[][] indexes=new int [100][100];
       boolean[][][] dp=new boolean[30][110][110];
        boolean[][][][] h=new boolean[51][110][110][10];
        BufferedReader input = new BufferedReader(
                new InputStreamReader(new FileInputStream("cf.in"), StandardCharsets.UTF_8));
        PrintWriter output = new PrintWriter(new BufferedWriter(
                new OutputStreamWriter(new FileOutputStream("cf.out"), StandardCharsets.UTF_8)));
        String str=input.readLine();
        String[] strx = str.split(" ");
        char begin=strx[1].charAt(0);
        int n=Integer.parseInt(strx[0]);;
        for(int i=0;i<30;i++){
            ArrayList<String> arl=new ArrayList<String>();
            edges[i]=arl;
        }
        for (int i = 0; i < n; i++) {
            String s=input.readLine();

            char from = s.charAt(0);
            String to="";

            if (s.charAt(s.length()-1) != '>') {
                int kek=0;
                for(int k=0;k<s.length();k++){
                    if(s.charAt(k)=='>'){
                        kek=k;
                        break;
                    }
                }
                for (int j = kek + 2; j < s.length(); j++) {
                    to += s.charAt(j);
                }
            } else {
                to = "";
            }

            rules.add(new Pair(from, to));
            edges[from - 'A'].add(to);
            indexes[from - 'A'][edges[from - 'A'].size() - 1] = rules.size() - 1;
        }

        String word=input.readLine();
        input.close();
        for (int i = 0; i < word.length(); i++) {
            for (int j = 0; j < rules.size(); j++) {
                Character x=rules.get(j).x;
                String y=rules.get(j).y;
                Pair rule=new Pair(x,y);

                dp[rule.x - 'A'][i][i + 1] = rule.y.length() == 1 && rule.y.charAt(0) == word.charAt(i);
                dp[rule.x - 'A'][i][i] = rule.y.isEmpty();
                h[j][i][i][0] = true;
            }
        }

        for (int cnt = 0; cnt < 26; cnt++) {
            for (int m = 0; m <= word.length(); m++) {
                for (int i = 0; i < 30; i++) {
                    int j = i + m;

                    if (j > word.length()) {
                        break;
                    }

                    for (int k = 1; k <= 5; k++) {
                        for (int ind = 0; ind < rules.size(); ind++) {
                            if (rules.get(ind).y.length() < k) {
                                continue;
                            }

                            for (int r = i; r <= j; r++) {
                                if (rules.get(ind).y.charAt(k - 1) >= 'a' && rules.get(ind).y.charAt(k - 1) <= 'z') {
                                    h[ind][i][j][k] |=
                                            h[ind][i][r][k - 1] && (r == j - 1) && (word.charAt(r) == rules.get(ind).y.charAt(k - 1));
                                } else {
                                    h[ind][i][j][k] |= h[ind][i][r][k - 1] && dp[rules.get(ind).y.charAt(k - 1) - 'A'][r][j];
                                }
                            }
                        }
                    }

                    for (int ind = 0; ind < 26; ind++) {
                        for (int ind1 = 0; ind1 < edges[ind].size(); ind1++) {
                            dp[ind][i][j] |= h[indexes[ind][ind1]][i][j][edges[ind].get(ind1).length()];
                        }
                    }
                }
            }
        }

        if (dp[begin - 'A'][0][word.length()]) {
            output.print("yes");
        } else {
            output.print( "no");
        }
        output.close();
    }
}
