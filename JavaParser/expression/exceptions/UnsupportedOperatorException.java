package expression.exceptions;

public class UnsupportedOperatorException extends ParsingException {
    public UnsupportedOperatorException(String message, int pos) {
        super("Unsupported operator at pos:" + pos + " " + message);
    }
}
