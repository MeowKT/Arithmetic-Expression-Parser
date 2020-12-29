package expression.exceptions;

public class MissingArgumentException extends ParsingException {
    public MissingArgumentException(Character c, int pos) {
        super("Expected argument at pos: " + pos + ", actual: " + c);
    }
}
