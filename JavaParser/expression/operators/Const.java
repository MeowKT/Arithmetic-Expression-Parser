package expression.operators;

import expression.operation.Operation;

public class Const<T> implements GenericExpression<T> {
    private String val;
    private Operation<T> op;

    public Const(String val, Operation op) {
        this.val = val;
        this.op = op;
    }

    @Override
    public T evaluate(T x, T y, T z) {
        return op.parse(val);
    }

    @Override
    public String toString() {
        return val;
    }

}
