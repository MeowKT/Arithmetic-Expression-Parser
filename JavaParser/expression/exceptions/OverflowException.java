package expression.exceptions;

public class OverflowException extends ComputationalException {
    public OverflowException(String op) {
        super(op + " Overflow");
    }
}