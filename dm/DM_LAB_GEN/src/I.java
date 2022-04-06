import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.StringTokenizer;

public class I {
    private static PrintWriter pw;
    private static BufferedReader br;
    static long MOD = 104857601L;

    static void getNegative(long[] Q, long[] NQ) {
        for (int i = 0; i < Q.length; ++i) {
            NQ[i] = (i % 2 == 0 ? Q[i] : (-Q[i] + MOD) % MOD);
        }
    }



    static void filter(long[] P, long n) {
        int freeI = 0;
        for (int i = 0; i < P.length; ++i) {
            if (i % 2 == n % 2) {
                P[freeI] = P[i];
                freeI++;
            }
        }
    }

    static long getNth(long[] P, long[] Q, int k, long n) {
        long[] R = new long[k + 1];
        long[] NQ = new long[k + 1];

        while (n >= k) {
            for (int i = k; i < P.length; ++i) {
                P[i] = 0;
                for (int j = 1; j < Q.length; ++j) {
                    P[i] = (P[i] - Q[j] * P[i - j]) % MOD;
                    P[i] %= MOD;
                    if (P[i] < 0) {
                        P[i] += MOD;
                    }
                }
            }

            getNegative(Q, NQ);

            for (int i = 0; i <= 2 * k; i += 2) {
                long coefR = 0;
                for (int j = 0; j <= i; ++j) {
                    long qq = (j > k ? 0 : Q[j]);
                    long neq = (i - j > k ? 0 : NQ[i - j]);

                    coefR = ((coefR + qq * neq)%MOD + MOD) % MOD;
                    coefR%=MOD;
                }
                R[i / 2] = coefR;
            }

            System.arraycopy(R,0,Q,0,R.length);
            filter(P, n);

            n = n / 2;
        }
        return P[(int) n];
    }


    public static void main(String[] args) throws IOException {
        br = new BufferedReader(new InputStreamReader(System.in));
        pw = new PrintWriter(System.out);
        int k = Integer.parseInt(nextToken());
        long n = Long.parseLong(nextToken())-1;
        long[] P = new long[2 * k];
        long[] Q = new long[k + 1];
        for (int i = 0; i < P.length / 2; ++i) {
            long a = Long.parseLong(nextToken());
            P[i] = a;
        }

        Q[0] = 1;
        for (int i = 1; i < Q.length; ++i) {
            long c = Long.parseLong(nextToken());
            Q[i] = (-c + MOD);
            Q[i] %= MOD;
        }

        pw.write(Long.toString(getNth(P, Q, k, n)));
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
