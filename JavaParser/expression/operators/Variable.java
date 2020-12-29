package expression.operators;

import expression.operation.Operation;

public class Variable<T> implements GenericExpression<T> {
    private String var;

    public Variable(String var) {
        this.var = var;
    }

    @Override
    public T evaluate(T x, T y, T z) {
        switch (var) {
            case("x") : return x;
            case("y") : return y;
            default: return z;
        }

    }

    @Override
    public String toString() {
        return var;
    }

}
