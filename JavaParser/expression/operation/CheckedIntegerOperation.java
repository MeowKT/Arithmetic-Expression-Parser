package expression.operation;


import expression.exceptions.IllegalOperationException;
import expression.exceptions.OverflowException;

import static java.lang.Integer.signum;
import static java.lang.Math.abs;


public class CheckedIntegerOperation extends IntegerOperation {

    @Override
    public Integer add(final Integer x, final Integer y) {
        if (y > 0 && Integer.MAX_VALUE - y < x) {
            throw new OverflowException("Add");
        } else if (y < 0 && Integer.MIN_VALUE - y > x) {
            throw new OverflowException("Add");
        }
        return x + y;
    }

    @Override
    public Integer subtract(final Integer x, final Integer y) {
        if (y < 0 && x > Integer.MAX_VALUE + y) {
            throw new OverflowException("Subtract");
        } else if (y > 0 && x < Integer.MIN_VALUE + y) {
            throw new OverflowException("Subtract");
        }
        return x - y;
    }


    @Override
    public Integer negate(final Integer x) {
        return multiply(x, -1);
    }

    @Override
    public Integer divide(final Integer x, final Integer y) {
        if (y == 0) {
            throw new IllegalOperationException("Divide by zero");
        }
        if (x == Integer.MIN_VALUE && y == -1) {
            throw new OverflowException("Divide");
        }
        return x / y;
    }

    @Override
    public Integer multiply(final Integer x, final Integer y) {
        if (x == 0 || y == 0 || x == 1 || y == 1) {
            return x * y;
        }

        if (y == Integer.MIN_VALUE || x == Integer.MIN_VALUE) {
            throw new OverflowException("Multiply");
        }
        final int sign = signum(x) * signum(y);
        if (sign > 0) {
            if (abs(x) > Integer.MAX_VALUE / abs(y)) {
                throw new OverflowException("Multiply");
            }
        } else {
            if (sign * abs(x) < Integer.MIN_VALUE / abs(y)) {
                throw new OverflowException("Multiply");
            }
        }
        return x * y;
    }

}
