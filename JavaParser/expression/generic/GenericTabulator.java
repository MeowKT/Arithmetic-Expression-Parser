package expression.generic;


import expression.exceptions.ComputationalException;
import expression.operation.*;
import expression.operation.Operation;
import expression.operators.GenericExpression;
import expression.parser.ExpressionParser;

import java.util.Map;

public class GenericTabulator implements Tabulator {
    private static final Map<String, Operation<?>> MODES = Map.of(
            "i", new CheckedIntegerOperation(),
            "d", new DoubleOperation(),
            "bi", new BigIntegerOperation(),
            "u", new IntegerOperation(),
            "f", new FloatOperation(),
            "b", new ByteOperation()
    );

    @Override
    public Object[][][] tabulate(final String mode, final String expression, final int x1, final int x2, final int y1, final int y2, final int z1, final int z2) {
        return makeTable(get(mode), expression, x1, x2, y1, y2, z1, z2);
    }

    private static Operation<?> get(final String mode) {
        final Operation<?> op = MODES.get(mode);
        if (op == null) {
            throw new ComputationalException("No mode:" + mode);
        }
        return op;
    }

    private <T> Object[][][] makeTable(final Operation<T> operator, final String expression, final int x1, final int x2, final int y1, final int y2, final int z1, final int z2) {
        final ExpressionParser<T> parser = new ExpressionParser<>(operator);
        final GenericExpression<T> genericExpression = parser.parse(expression);
        final Object[][][] ans = new Object[x2 - x1 + 1][y2 - y1 + 1][z2 - z1 + 1];
        for (int x = 0; x <= x2 - x1; x++) {
            for (int y = 0; y <= y2 - y1; y++) {
                for (int z = 0; z <= z2 - z1; z++) {
                    try {
                        ans[x][y][z] = genericExpression.evaluate(
                                operator.parse(x + x1),
                                operator.parse(y + y1),
                                operator.parse(z + z1));
                    } catch (final ComputationalException ignored) {}
                }
            }
        }
        return ans;
    }
}