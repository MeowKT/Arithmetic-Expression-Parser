package expression.parser;

import expression.exceptions.ParsingException;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class BaseParser {
    private ExpressionSource source;
    protected char ch;
    protected int pos = 0;

    protected void setSource(ExpressionSource source) {
        this.source = source;
    }

    protected void nextChar() {
        if (source.hasNext()) {
            ch = source.next();
            pos++;
        } else {
            ch = '\0';
        }
    }

    protected boolean test(char expected) {
        if (ch == expected) {
            nextChar();
            return true;
        }
        return false;
    }

    protected void expect(final char c) {
        if (ch != c) {
            throw error("Expected '" + c + "', found '" + ch + "'");
        }
        nextChar();
    }

    protected void expect(final String value) {
        if (value.equals("")) return;
        for (char c : value.toCharArray()) {
            expect(c);
        }
    }

    protected boolean between(final char from, final char to) {
        return from <= ch && ch <= to;
    }

    protected ParsingException error(String message) {
        return source.error(message);
    }
}
