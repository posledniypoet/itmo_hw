import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.StringTokenizer;

public class A {
    static long MOD = 998244353L;
    private static PrintWriter pw;
    private static BufferedReader br;

    static long MOD(long x) {
        return (x % MOD + MOD) % MOD;
    }

    public static void main(String[] args) throws IOException {
        br = new BufferedReader(new InputStreamReader(System.in));
        pw = new PrintWriter(System.out);
        int n = Integer.parseInt(nextToken());
        int m = Integer.parseInt(nextToken());
        ArrayList<Long> P = new ArrayList<Long>();
        ArrayList<Long> Q = new ArrayList<Long>();
        for (int i = 0; i <= n; ++i) {
            long x = Long.parseLong(nextToken());
            P.add(x);
        }

        for (int i = 0; i <= m; ++i) {
            long y = Long.parseLong(nextToken());
            Q.add(y);
        }
        int z = n;
        while (z != 1001) {
            P.add(0L);
            z++;
        }
        z = m;
        while (z != 1001) {
            Q.add(0L);
            z++;
        }


        pw.println(Math.max(n, m));
        for (int i = 0; i <= Math.max(n, m); ++i) {
            pw.print(MOD(P.get(i) + Q.get(i)) + " ");
        }
        pw.println("\n" + (n + m));
        for (int i = 0; i < n + m + 1; ++i) {
            long x = 0;
            for (int j = 0; j < i + 1; ++j) {
                if (j >= 1001 || i - j >= 1001)
                    continue;

                x = MOD(x + MOD(P.get(j) * Q.get(i - j)));
            }

            pw.print(x + " ");
        }
        pw.print("\n");
        ArrayList<Long> V = new ArrayList<Long>();
        for (int i = 0; i < 1000; ++i) {
            long x = 0;
            for (int j = 0; j < i; ++j) {
                x = MOD(x + V.get(j) * Q.get(i - j));
            }

            V.add(MOD(P.get(i) - x));
        }
        for (int i = 0; i < V.size(); i++) {
            pw.print(V.get(i) + " ");
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


