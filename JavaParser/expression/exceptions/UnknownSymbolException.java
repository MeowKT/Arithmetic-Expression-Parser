package expression.exceptions;

public class UnknownSymbolException extends ComputationalException {
    public UnknownSymbolException(String message, int pos) {
        super("Unknown symbol at pos " + pos + " " + message);
    }
}
