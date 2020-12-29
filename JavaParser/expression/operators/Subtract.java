package expression.operators;

import expression.operation.Operation;

public class Subtract<T> extends AbstractBinaryOperator<T> {

    public Subtract(GenericExpression<T> left, GenericExpression<T> right, Operation<T> op) {
        super(left, right, op);
    }
    @Override
    protected String getOperator() {
        return " - ";
    }

    @Override
    protected T calc(T x, T y) {
        return op.subtract(x, y);
    }
}
