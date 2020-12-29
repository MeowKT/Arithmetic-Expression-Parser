package expression.operators;

import expression.exceptions.OverflowException;
import expression.operation.Operation;

public class Count<T> extends AbstractUnaryOperator<T> {

    public Count(GenericExpression<T> expression, Operation<T> op) {
        super(expression, op);
    }

    @Override
    public T operate(T x) throws OverflowException {
        return op.count(x);
    }

    @Override
    protected String getOperator() {
        return "count";
    }

}
