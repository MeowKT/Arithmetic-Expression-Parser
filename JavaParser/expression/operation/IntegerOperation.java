package expression.operation;


import expression.exceptions.IllegalOperationException;


public class IntegerOperation implements Operation<Integer> {

    @Override
    public Integer add(final Integer x, final Integer y) {
        return x + y;
    }

    @Override
    public Integer subtract(final Integer x, final Integer y) {
        return x - y;
    }


    @Override
    public Integer negate(final Integer x) {
        return multiply(x, -1);
    }

    @Override
    public Integer parse(final String x) {
        return Integer.parseInt(x);
    }

    @Override
    public Integer parse(final int x) {
        return x;
    }

    @Override
    public Integer min(final Integer x, final Integer y) {
        return Integer.min(x, y);
    }

    @Override
    public Integer max(final Integer x, final Integer y) {
        return Integer.max(x, y);
    }

    @Override
    public Integer count(final Integer x) {
        return Integer.bitCount(x);
    }

    @Override
    public Integer divide(final Integer x, final Integer y) {
        if (y == 0) {
            throw new IllegalOperationException("Divide by zero");
        }
        return x / y;
    }

    @Override
    public Integer multiply(final Integer x, final Integer y) {
        return x * y;
    }

}
