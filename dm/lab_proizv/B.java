import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.StringTokenizer;

public class B {
    static long MOD = 998244353L;
    private static PrintWriter pw;
    private static BufferedReader br;

    static long getReal(int ind, long[] v) {
        if (ind >= v.length) {
            return 0;
        }

        return v[ind];
    }

    static long binMul(long a, int n) {
        if (n == 1) {
            return a;
        }

        if (n % 2 == 1) {
            return (a * binMul(a, n - 1)) % MOD;
        } else {
            long tmp = binMul(a, n / 2);
            return (tmp * tmp) % MOD;
        }
    }
    static long getReverse(long a) {
        return binMul(a, (int)MOD - 2);
    }

    static long[] mulPol(long[]p, long[]q, int limit) {
        int n = p.length;
        int m = q.length;

        int size = Math.min(n + m + 3, limit);
        long[] mul =new long [size];
        for (int i = 0; i < size; ++i) {
            for (int j = 0; j <= i; ++j) {
                mul[i] += (getReal(j, p) * getReal(i - j, q)) % MOD;
                mul[i] %= MOD;
            }
        }

        int st = size - 1;
        while (st > 0 && mul[st] == 0) {
            --st;
        }
        System.arraycopy(mul,0,mul,0,st+1);
        return mul;
    }

    static long binCoef(long n) {
        long a = 1;
        long b = 1;
        --n;
        for (int i = 0; i <= n; ++i) {
            a *= (1 - 2 * i + MOD);
            a %= MOD;
            b *= ((i + 1) * 2) % MOD;
            b %= MOD;
        }

        return (a * getReverse(b)) % MOD;
    }
    public static void main(String[] args) throws IOException {
        br = new BufferedReader(new InputStreamReader(System.in));
        pw = new PrintWriter(System.out);
        int n = Integer.parseInt(nextToken());
        int m = Integer.parseInt(nextToken());
        long[] P = new long[n+1];
        for (int i = 0; i <= n; ++i) {
            long p = Long.parseLong(nextToken());
            P[i]=p;
        }
        long[] SQRT = new long[m];
        long[] EXP = new long[m];
        long[] LOG = new long[m];
        SQRT[0] = EXP[0] = 1;
        long[] tmp = new long[1];
        tmp[0]=1;
        long fact = 1;
        long lnCoef = -1 + MOD;
        for (int i = 1; i < m; ++i) {
            tmp = mulPol(tmp, P, m);

            long bCoef = binCoef(i);
            if (bCoef < 0) {
                bCoef += MOD;
            }

            fact *= i;
            fact %= MOD;

            lnCoef *= -1;
            lnCoef += MOD;
            for (int j = 0; j < m; ++j) {
                SQRT[j] += (bCoef * getReal(j, tmp)) % MOD;
                SQRT[j] %= MOD;

                EXP[j] += ((1 * getReverse(fact)) % MOD * getReal(j, tmp)) % MOD;
                EXP[j] %= MOD;

                LOG[j] += ((lnCoef * getReverse(i)) % MOD * getReal(j, tmp)) % MOD;
                LOG[j] %= MOD;
            }
        }

        for (int i=0;i< SQRT.length;i++) {
            pw.write(SQRT[i]+" ");
        }
        pw.write("\n");

        for (int i=0;i< EXP.length;i++) {
            pw.write(EXP[i]+" ");
        }
        pw.write("\n");

        for (int i=0;i< LOG.length;i++) {
            pw.write(LOG[i]+" ");
        }
        pw.write("\n");
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


