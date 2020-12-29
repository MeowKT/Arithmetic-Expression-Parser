package expression.parser;

import java.util.Map;

public enum BinaryOperator {
    Add, Sub, Mul, Div, Min, Max, UNDEFINED;
    private static final Map<BinaryOperator, String> operatorToString = Map.of(
            Add, "+",
            Sub, "-",
            Mul, "*",
            Div, "/",
            Min, "min",
            Max, "max"
    );

    private static final Map<BinaryOperator, Integer> OPERATOR_LVL = Map.of(
            UNDEFINED, 3,
            Add, 1,
            Sub, 1,
            Mul, 2,
            Div, 2,
            Min, 0,
            Max, 0
    );

    public int getLvl() {
        return OPERATOR_LVL.get(this);
    }

    @Override
    public String toString() {
        return operatorToString.get(this);
    }

}
