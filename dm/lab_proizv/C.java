import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.StringTokenizer;

public class C {

    private static PrintWriter pw;
    private static BufferedReader br;
    static long MODULE = 1000000000000L;

    static int recurrent_value(ArrayList<Integer> A, ArrayList<Integer> C, int k) {
        int value = 0;
        for (int i = 0; i < k; ++i)
            value += C.get(i) * A.get(k - i - 1);
        return value;
    }


    public static void main(String[] args) throws IOException {
        br = new BufferedReader(new InputStreamReader(System.in));
        pw = new PrintWriter(System.out);
        int k = Integer.parseInt(nextToken());
        ArrayList<Integer> A = new ArrayList<Integer>();
        ArrayList<Integer> C = new ArrayList<Integer>();
        ArrayList<Integer> P = new ArrayList<Integer>();
        for (int i = 0; i < k; ++i) {
            int a = Integer.parseInt(nextToken());
            A.add(a);
        }

        for (int i = 0; i < k; ++i) {
            int c = Integer.parseInt(nextToken());
            C.add(c);
        }

        for (int i = 0; i < k; ++i){
            P.add(A.get(i)-recurrent_value(A, C, i));
        }
        P.add(0);

        int degree = k + 1;
        while (degree >= 0 && P.get(degree-1) == 0)
            --degree;

        pw.write(degree-1 + "\n");
        for (int i = 0; i < degree; ++i)
            pw.write(P.get(i)+ " ");

        pw.write("\n" + k + "\n" + "1" + " ");
        for (int i =0 ;i<C.size();i++)
            pw.write(-C.get(i) + " ");
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
