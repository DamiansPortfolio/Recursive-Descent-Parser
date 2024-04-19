
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public enum TokenSet {
    LET("let"),
    READ("read"),
    WRITE("write"),
    VAR("var"),
    L_PAREN("("),
    R_PAREN(")"),
    EQUAL("="),
    SUBR_OP("<-"),
    COMMA(","),
    ADD_OP("-", "+"),
    MULT_OP("*", "/"),
    REL_OP(">", "<", "=="),

    UNTIL("until"),
    REPEAT("repeat"),
    IF("if"),
    THEN("then"),
    ELSE("else"),
    END_IF("endif"),

    $$, // End of file

    ID(),
    NUMBER(); // A sequence of digits.

    /**
     * A list of all lexemes for each token.
     */
    private final List<String> lexemeList;

    TokenSet(final String... tokenStrings) {
        this.lexemeList = new ArrayList<>(tokenStrings.length);
        this.lexemeList.addAll(Arrays.asList(tokenStrings));
    }

    /**
     * Get a TokenSet object from the Lexeme string.
     *
     * @param string The String (lexeme) to convert to a compiler.TokenSet
     * @return A compiler.TokenSet object based on the input String (lexeme)
     */
    static TokenSet getTokenFromLexeme(final String string) {
        // Just to be safe…
        final var lexeme = string.trim();

        // An empty string/lexeme should mean no more tokens to process.
        // Return the "end of input maker" if the string is empty.
        if (lexeme.isEmpty()) {
            return $$;
        }

        // Regex for one or more digits optionally followed by and more digits.
        // (doesn't handle "-", "+" etc., only digits)
        // Return the number token if the string represents a number.
        if (lexeme.matches(LexicalAnalyzer.NUMBER_REGEX)) {
            return ID;
        }

        // Search through ALL lexemes looking for a match with early bailout.
        // Return the matching token if found.
        for (var token : TokenSet.values()) {
            if (token.lexemeList.contains(lexeme)) {
                // early bailout from the loop.
                return token;
            }
        }

        // Return "ID" if no match was found.
        return ID;
    }
}
