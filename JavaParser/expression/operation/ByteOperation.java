package expression.operation;


import expression.exceptions.IllegalOperationException;


public class ByteOperation implements Operation<Byte> {

    @Override
    public Byte add(Byte x, Byte y) {
        return (byte)(x + y);
    }

    @Override
    public Byte subtract(Byte x, Byte y) {
        return (byte)(x - y);
    }


    @Override
    public Byte negate(Byte x) {
        return (byte)-x;
    }

    @Override
    public Byte parse(String x) {
        return (byte)Integer.parseInt(x);
    }

    @Override
    public Byte parse(int x) {
        return (byte) x;
    }

    @Override
    public Byte min(Byte x, Byte y) {
        return x < y ? x : y;
    }

    @Override
    public Byte max(Byte x, Byte y) {
        return x < y ? y : x;
    }

    @Override
    public Byte count(Byte x) {
        return x == -1 ? 8 : (byte)(Integer.bitCount(x) % 8);
    }

    @Override
    public Byte divide(Byte x, Byte y) {
        if (y == 0) {
            throw new IllegalOperationException("Divide by zero");
        }
        return (byte)(x / y);
    }

    @Override
    public Byte multiply(Byte x, Byte y) {
        return (byte)(x * y);
    }

}
