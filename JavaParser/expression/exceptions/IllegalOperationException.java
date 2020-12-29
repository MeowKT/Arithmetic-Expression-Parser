package expression.exceptions;

public class IllegalOperationException extends ComputationalException {
    public IllegalOperationException(String message) {
        super("Illegal expression.operation " + message);
    }
}
