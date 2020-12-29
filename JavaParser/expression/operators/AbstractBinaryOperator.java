package expression.operators;

import expression.exceptions.ComputationalException;
import expression.operation.Operation;

abstract class AbstractBinaryOperator <T> implements GenericExpression <T> {
    private GenericExpression<T> left;
    private GenericExpression<T> right;
    protected Operation<T> op;

    protected abstract T calc(T x, T y) throws ComputationalException;

    protected abstract String getOperator();

    public AbstractBinaryOperator(GenericExpression<T> left, GenericExpression<T> right, Operation<T> op) {
        this.left = left;
        this.right = right;
        this.op = op;
    }

    public T evaluate(T x, T y, T z) throws ComputationalException {
        return calc(left.evaluate(x, y, z), right.evaluate(x, y, z));
    }

    @Override
    public String toString() {
        return "(" + left + getOperator() + right + ")";
    }

}
