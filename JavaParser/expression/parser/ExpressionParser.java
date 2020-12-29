package expression.parser;

import expression.exceptions.*;
import expression.operation.Operation;
import expression.operators.*;

import java.util.Map;

public class ExpressionParser<T> extends BaseParser {

    private final Operation<T> mode;

     private static final Map<String, BinaryOperator> PREFIX_TO_BINARY_OPERATOR = Map.of(
            "", BinaryOperator.UNDEFINED,
            "+", BinaryOperator.Add,
            "-", BinaryOperator.Sub,
            "*", BinaryOperator.Mul,
            "/", BinaryOperator.Div,
             "m", BinaryOperator.Min,
             "ma", BinaryOperator.Max
     );

     private static Map<String, UnaryOperator> prefixToUnaryOperator = Map.of(
             "", UnaryOperator.Undefined,
             "-", UnaryOperator.Minus,
             "c", UnaryOperator.BitCount
     );

    private BinaryOperator lastBinaryOperator = BinaryOperator.UNDEFINED;
    private UnaryOperator lastUnaryOperator = UnaryOperator.Undefined;
    private static final int MAX_LEVEL = 3;

    public ExpressionParser(final Operation<T> mode) {
        this.mode = mode;
    }

    public GenericExpression<T> parse(final String expression) {
        setSource(new StringSource(expression));
        nextChar();
        final GenericExpression<T> result = parseExpression();
        if (test('\0')) {
            return result;
        }
        throw new UnknownSymbolException(ch + "", pos);
    }

    private GenericExpression<T> parseExpression() {
        return parseLevel(0);
    }

    private GenericExpression<T> parseLevel(final int level) {
        if (level == MAX_LEVEL) {
            return parseSimpleExpression();
        }
        GenericExpression<T> result = parseLevel(level + 1);
        while (hasBinaryOperationOnLevel(level)) {
            final BinaryOperator op = lastBinaryOperator;
            lastBinaryOperator = BinaryOperator.UNDEFINED;
            result = makeBinaryExpression(op, result, parseLevel(level + 1));
        }
        return result;
    }

    private boolean hasBinaryOperationOnLevel(final int level) {
        skipWhitespace();
        if (lastBinaryOperator == BinaryOperator.UNDEFINED) {
            final StringBuilder st = new StringBuilder();
            st.append(ch);
            while (PREFIX_TO_BINARY_OPERATOR.containsKey(st.toString())) {
                nextChar();
                st.append(ch);
            }
            st.setLength(st.length() - 1);
            lastBinaryOperator = PREFIX_TO_BINARY_OPERATOR.get(st.toString());
            if (st.length() > 0) {
                expect(lastBinaryOperator.toString().substring(st.length()));
            }
        }
        return lastBinaryOperator != BinaryOperator.UNDEFINED && lastBinaryOperator.getLvl() == level;
    }

    private boolean hasUnaryOperation() {
        skipWhitespace();
        if (lastUnaryOperator == UnaryOperator.Undefined) {
            final StringBuilder st = new StringBuilder();
            st.append(ch);
            while (prefixToUnaryOperator.containsKey(st.toString())) {
                nextChar();
                st.append(ch);
            }
            st.setLength(st.length() - 1);
            lastUnaryOperator = prefixToUnaryOperator.get(st.toString());
            if (st.length() > 0) {
                expect(lastUnaryOperator.toString().substring(st.length()));
            }
        }
        return lastUnaryOperator != UnaryOperator.Undefined;
    }

    private GenericExpression<T> parseSimpleExpression() {
        skipWhitespace();
        if (test('(')) {
            final GenericExpression<T> result = parseExpression();
            skipWhitespace();
            if (ch != ')') {
                throw new MissingBracketException("close", pos);
            }
            nextChar();
            return result;
        } else if (between('0', '9')) {
            return parseConstExpression(false);
        } else if (hasUnaryOperation()) {
            final UnaryOperator op = lastUnaryOperator;
            lastUnaryOperator = UnaryOperator.Undefined;
            if (between('0', '9')) {
                return parseConstExpression(true);
            } else if (op == UnaryOperator.Minus){
                return new Negate<>(parseSimpleExpression(), mode);
            }
            return makeUnaryExpression(op, parseSimpleExpression());
        } else {
            return parseVariableExpression();
        }
    }
    private GenericExpression<T> parseConstExpression(final boolean isNegative) {
        final StringBuilder st = new StringBuilder(isNegative ? "-" : "");
        while (between('0', '9')) {
            st.append(ch);
            nextChar();
        }
        try {
            return new Const<>(st.toString(), mode);
        } catch (final NumberFormatException e) {
            throw new IllegalConstantException("Constant overflow: " + st, pos);
        }
    }
    private GenericExpression<T> parseVariableExpression() {
        final StringBuilder st = new StringBuilder();
        while (between('x', 'z')) {
            st.append(ch);
            nextChar();
        }
        if (st.length() == 0) {
            throw new MissingArgumentException(ch, pos);
        }
        return new Variable<>(st.toString());
    }

    private GenericExpression<T> makeUnaryExpression(final UnaryOperator operator, final GenericExpression<T> a) {
        if (operator == UnaryOperator.BitCount) {
            return new Count<>(a, mode);
        }
        throw new UnsupportedOperatorException("unary operator: " + operator, pos);
    }

    private GenericExpression<T> makeBinaryExpression(final BinaryOperator operator, final GenericExpression<T> a, final GenericExpression<T> b) {
        switch (operator) {
            case Add : return new Add<>(a, b, mode);
            case Sub : return new Subtract<>(a, b, mode);
            case Mul : return new Multiply<>(a, b, mode);
            case Div : return new Divide<>(a, b, mode);
            case Min : return new Min<>(a, b, mode);
            case Max : return new Max<>(a, b, mode);
            default: throw new UnsupportedOperatorException("binary operator: " + operator, pos);
        }
    }

    private void skipWhitespace() {
        while (test(' ') || test('\r') || test('\n') || test('\t')) {

        }
    }
}