package expression.parser;

import java.util.Map;

public enum UnaryOperator {
    Minus, BitCount, Undefined;
    private static final Map<UnaryOperator, String> operatorToString = Map.of(
            Undefined, "",
            Minus, "-",
            BitCount, "count"
    );

    @Override
    public String toString() {
        return operatorToString.get(this);
    }

}
