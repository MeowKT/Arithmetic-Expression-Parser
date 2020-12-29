package expression.operators;

import expression.exceptions.OverflowException;
import expression.operation.Operation;

public class Negate<T> extends AbstractUnaryOperator<T> {

    public Negate(GenericExpression<T> expression, Operation<T> op) {
        super(expression, op);
    }

    @Override
    public T operate(T x) throws OverflowException {
        return op.negate(x);
    }

    @Override
    protected String getOperator() {
        return "-";
    }

}
