import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.StringTokenizer;

public class F {
    private static PrintWriter pw;
    private static BufferedReader br;
    static long MODULE = 1000000007L;


    public static void main(String[] args) throws IOException {
        br = new BufferedReader(new InputStreamReader(System.in));
        pw = new PrintWriter(System.out);
        int k = Integer.parseInt(nextToken());
        int m = Integer.parseInt(nextToken());
        ArrayList<Long> C = new ArrayList<Long>();
        ArrayList<Long> ANS = new ArrayList<Long>();
        ArrayList<Long> SUM = new ArrayList<Long>();
        for (int i = 0; i < k; ++i) {
            long c = Long.parseLong(nextToken());
            C.add(c);
        }
        ANS.add(1L);
        SUM.add(1L);
        for (int i =0;i<m;i++){
            ANS.add(0L);
            SUM.add(0L);
        }

        for (int i = 1; i <= m; ++i) {
            for (int j = 0; j < k; ++j) {
                if (i >= C.get(j)) {
                    ANS.set(i,ANS.get(i)+SUM.get((int) (i-C.get(j))));
                    ANS.set(i,ANS.get(i)%MODULE);
                }
            }

            for(int j = 0; j <= i; ++j) {
                SUM.set(i,SUM.get(i)+(ANS.get(j)*ANS.get(i-j)%MODULE));
                SUM.set(i,SUM.get(i)%MODULE);
            }
        }

        for (int i = 1; i <= m; ++i) {
            pw.write(ANS.get(i)+" ");
        }
        pw.close();


    }
    static StringTokenizer st;

    public static String nextToken() throws IOException {
        while (st == null || !st.hasMoreTokens()) {
            st = new StringTokenizer(br.readLine());
        }
        return st.nextToken();
    }

}
