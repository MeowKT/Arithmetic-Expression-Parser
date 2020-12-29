package expression.exceptions;

public class IllegalConstantException extends ParsingException {
    public IllegalConstantException(String message, int pos) {
        super(message + " at pos " + pos);
    }
}
