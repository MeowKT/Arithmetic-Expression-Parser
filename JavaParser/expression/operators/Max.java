package expression.operators;

import expression.operation.Operation;

public class Max<T> extends AbstractBinaryOperator<T>  {

    public Max(GenericExpression<T> left, GenericExpression<T> right, Operation<T> op) {
        super(left, right, op);
    }
    @Override
    protected String getOperator() {
        return " max ";
    }

    @Override
    protected T calc(T x, T y) {
        return op.max(x, y);
    }

}
