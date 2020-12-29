package expression.operation;


public class FloatOperation implements Operation<Float> {

    @Override
    public Float add(Float x, Float y) {
        return x + y;
    }

    @Override
    public Float subtract(Float x, Float y) {
        return x - y;
    }

    @Override
    public Float multiply(Float x, Float y) {
        return x * y;
    }

    @Override
    public Float divide(Float x, Float y) {
        return x / y;
    }

    @Override
    public Float negate(Float x) {
        return -x;
    }

    @Override
    public Float parse(String x) {
        return Float.parseFloat(x);
    }

    @Override
    public Float parse(int x) {
        return (float) x;
    }

    @Override
    public Float min(Float x, Float y) {
        return Float.min(x, y);
    }

    @Override
    public Float max(Float x, Float y) {
        return Float.max(x, y);
    }

    @Override
    public Float count(Float x) {
        return (float)Integer.bitCount(Float.floatToIntBits(x));
    }
}
