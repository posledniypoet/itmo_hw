import org.jetbrains.annotations.NotNull;

/**
 * В теле класса решения разрешено использовать только финальные переменные типа RegularInt.
 * Нельзя volatile, нельзя другие типы, нельзя блокировки, нельзя лазить в глобальные переменные.
 *
 * @author :TODO: LastName FirstName
 */
public class Solution implements MonotonicClock {
    private final RegularInt l1 = new RegularInt(0);
    private final RegularInt l2 = new RegularInt(0);
    private final RegularInt k1 = new RegularInt(0);
    private final RegularInt k2 = new RegularInt(0);
    private final RegularInt r = new RegularInt(0);

    @Override
    public void write(@NotNull Time time) {
        // write right-to-left
       l2.setValue(time.getD1());
       k2.setValue(time.getD2());
       r.setValue(time.getD3());
       k1.setValue(time.getD2());
       l1.setValue(time.getD1());
    }

    @NotNull
    @Override
    public Time read() {
        int u1 = l1.getValue();
        int v1 = k1.getValue();
        int w = r.getValue();
        int v2 = k2.getValue();
        int u2 = l2.getValue();
        if (u1 == u2) {
            if (v1 == v2) {
                return new Time(u2, v2, w);
            } else {
                return new Time(u2, v2, 0);
            }
        } else {
            return new Time(u2, 0, 0);
        }
    }
}