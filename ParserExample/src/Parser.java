import java.lang.Character.UnicodeScript;
import java.util.LinkedList;
import java.util.List;
import java.util.logging.Logger;

class Parser {

    // The lexer, which will provide the tokens
    private final LexicalAnalyzer lexer;

    // The "code generator"
    private final CodeGenerator codeGenerator;

    /**
     * This is the constructor for the Parser class which
     * accepts a LexicalAnalyzer, and a CodeGenerator object as parameters.
     *
     * @param lexer         The TokenSet Object
     * @param codeGenerator The CodeGenerator Object
     */
    Parser(LexicalAnalyzer lexer, CodeGenerator codeGenerator) {
        this.lexer = lexer;
        this.codeGenerator = codeGenerator;

        // Change this to automatically prompt to see the Open WebGraphViz dialog or
        // not.
        MAIN.PROMPT_FOR_GRAPHVIZ = true;
    }

    public void analyze(TreeNode treeRoot) {
        try {
            // THIS IS OUR START RULE
            PROGRAM(treeRoot);
        } catch (ParseException ex) {
            final String msg = String.format("%s\n", ex.getMessage());
            Logger.getAnonymousLogger().severe(msg);
        }
    }

    List<String> symbolTable = new LinkedList<>();

    // <PROGRAM> ::= <STMT_LIST> $$
    private void PROGRAM(final TreeNode parentNode) throws ParseException {
        final TreeNode thisNode = codeGenerator.addNonTerminalToTree(parentNode);

        // Invoke first rule
        STMT_LIST(thisNode);

        // Test for end of input
        MATCH(thisNode, TokenSet.$$);
    }

    // <STMT_LIST> ::= <STMT> <STMT_LIST> | ε
    private void STMT_LIST(final TreeNode parentNode) throws ParseException {
        final TreeNode thisNode = codeGenerator.addNonTerminalToTree(parentNode);

        switch (lexer.getCurrentToken()) {
            case READ, WRITE, VAR, ID, LET, IF, UNTIL -> {
                STMT(thisNode);
                STMT_LIST(thisNode);
            }
            default -> EPSILON(thisNode);
        }
    }

    // <EPSILON> ::= ε
    private void EPSILON(final TreeNode parentNode) throws ParseException {
        final TreeNode thisNode = codeGenerator.addNonTerminalToTree(parentNode);
        EMPTY(thisNode);
    }

    // <STMT> ::= <READ_STMT> | <WRITE_STMT> | <VAR_DECL> | <SUBR_CALL> | let id
    // <ASGN_STMT> | <IF_STMT> | <UNTIL_STMT>
    private void STMT(final TreeNode parentNode) throws ParseException {
        final TreeNode thisNode = codeGenerator.addNonTerminalToTree(parentNode);

        switch (lexer.getCurrentToken()) {
            case READ -> READ_STMT(thisNode);
            case WRITE -> WRITE_STMT(thisNode);
            case ID -> SUBR_CALL(thisNode);
            case VAR -> VAR_DECL(thisNode);
            case LET -> LET_STMT(thisNode);
            case IF -> IF_STMT(thisNode);
            case UNTIL -> UNTIL_STMT(thisNode);
            default -> codeGenerator.syntaxError("Illegal start of statement!", parentNode);
        }
    }

    // <UNTIL_STMT> ::= UNTIL <CONDITION> <STMT_LIST> REPEAT
    private void UNTIL_STMT(final TreeNode parentNode) throws ParseException {
        final TreeNode thisNode = codeGenerator.addNonTerminalToTree(parentNode);
        MATCH(thisNode, TokenSet.UNTIL);
        CONDITION(thisNode);
        STMT_LIST(thisNode);
        MATCH(thisNode, TokenSet.REPEAT);
    }

    // <ELSE> ::= ELSE <STMT_LIST>
    private void ELSE(final TreeNode parentNode) throws ParseException {
        final TreeNode thisNode = codeGenerator.addNonTerminalToTree(parentNode);
        MATCH(thisNode, TokenSet.ELSE);
        STMT_LIST(thisNode);
    }

    // <IF_STMT> ::= IF <CONDITION> THEN <STMT_LIST> ELSE (optional) END_IF
    private void IF_STMT(final TreeNode parentNode) throws ParseException {
        final TreeNode thisNode = codeGenerator.addNonTerminalToTree(parentNode);
        MATCH(thisNode, TokenSet.IF);
        CONDITION(thisNode);
        MATCH(thisNode, TokenSet.THEN);
        STMT_LIST(thisNode);
        if (lexer.getCurrentToken() == TokenSet.ELSE) {
            ELSE(thisNode);
        }
        MATCH(thisNode, TokenSet.END_IF);
    }

    // <LET_STMT> ::= LET ID <ASGN_STMT>
    private void LET_STMT(final TreeNode parentNode) throws ParseException {
        final TreeNode thisNode = codeGenerator.addNonTerminalToTree(parentNode);
        MATCH(thisNode, TokenSet.LET);
        if (symbolTable.contains(lexer.getCurrentLexeme())) {
            MATCH(thisNode, TokenSet.ID);
        } else {
            codeGenerator.syntaxError("Variable not declared", parentNode);
        }
        ASGN_STMT(thisNode);
    }

    // <VAR_DECL> ::= VAR ID
    private void VAR_DECL(final TreeNode parentNode) throws ParseException {
        final TreeNode thisNode = codeGenerator.addNonTerminalToTree(parentNode);
        MATCH(thisNode, TokenSet.VAR);
        if (!symbolTable.contains(lexer.getCurrentLexeme())) {
            symbolTable.add(lexer.getCurrentLexeme());
            MATCH(thisNode, TokenSet.ID);
        } else {
            codeGenerator.syntaxError("Variable already declared", parentNode);
        }
    }

    // <READ_STMT> ::= READ ID
    private void READ_STMT(final TreeNode parentNode) throws ParseException {
        final TreeNode thisNode = codeGenerator.addNonTerminalToTree(parentNode);
        MATCH(thisNode, TokenSet.READ);
        if (symbolTable.contains(lexer.getCurrentLexeme())) {
            MATCH(thisNode, TokenSet.ID);
        } else {
            codeGenerator.syntaxError("Variable not declared", parentNode);
        }
    }

    // <WRITE_STMT> ::= WRITE <EXPR>
    private void WRITE_STMT(final TreeNode parentNode) throws ParseException {
        final TreeNode thisNode = codeGenerator.addNonTerminalToTree(parentNode);
        MATCH(thisNode, TokenSet.WRITE);
        EXPR(thisNode);
    }

    // <ASGN_STMT> ::= EQUAL <EXPR> | SUBR_OP <SUBR_CALL>
    private void ASGN_STMT(final TreeNode parentNode) throws ParseException {
        final TreeNode thisNode = codeGenerator.addNonTerminalToTree(parentNode);
        if (lexer.getCurrentToken() == TokenSet.EQUAL) {
            MATCH(thisNode, TokenSet.EQUAL);
            EXPR(thisNode);
        } else if (lexer.getCurrentToken() == TokenSet.SUBR_OP) {
            MATCH(thisNode, TokenSet.SUBR_OP);
            SUBR_CALL(thisNode);
        } else {
            codeGenerator.syntaxError("Illegal assignment statement!", parentNode);
        }
    }

    // <EXPR> ::= <TERM> <TERM_TAIL>
    private void EXPR(final TreeNode parentNode) throws ParseException {
        final TreeNode thisNode = codeGenerator.addNonTerminalToTree(parentNode);
        TERM(thisNode);
        TERM_TAIL(thisNode);
    }

    // <SUBR_CALL> ::= ID L_PAREN <ARG_LIST> R_PAREN
    private void SUBR_CALL(final TreeNode parentNode) throws ParseException {
        final TreeNode thisNode = codeGenerator.addNonTerminalToTree(parentNode);
        if (symbolTable.contains(lexer.getCurrentLexeme())) {
            MATCH(thisNode, TokenSet.ID);
        } else {
            codeGenerator.syntaxError("Variable not declared.", parentNode);
        }
        MATCH(thisNode, TokenSet.L_PAREN);
        ARG_LIST(thisNode);
        MATCH(thisNode, TokenSet.R_PAREN);
    }

    // <TERM> ::= <FACTOR> <FACTOR_TAIL>
    private void TERM(final TreeNode parentNode) throws ParseException {
        final TreeNode thisNode = codeGenerator.addNonTerminalToTree(parentNode);
        FACTOR(thisNode);
        FACTOR_TAIL(thisNode);
    }

    // <FACTOR> ::= L_PAREN <EXPR> R_PAREN | ID
    private void FACTOR(final TreeNode parentNode) throws ParseException {
        final TreeNode thisNode = codeGenerator.addNonTerminalToTree(parentNode);

        if (lexer.getCurrentToken() == TokenSet.L_PAREN) {
            MATCH(thisNode, TokenSet.L_PAREN);
            EXPR(thisNode);
            MATCH(thisNode, TokenSet.R_PAREN);
        } else if (lexer.getCurrentToken() == TokenSet.ID || lexer.getCurrentToken() == TokenSet.NUMBER) {
            if (symbolTable.contains(lexer.getCurrentToken())) {
                MATCH(thisNode, TokenSet.ID);
            } else {
                codeGenerator.syntaxError("Variable not declared.", parentNode);
            }
        }
    }

    // <FACTOR_TAIL> ::= MUTL_OP <FACTOR> <FACTOR_TAIL> | ε
    private void FACTOR_TAIL(final TreeNode parentNode) throws ParseException {
        final TreeNode thisNode = codeGenerator.addNonTerminalToTree(parentNode);

        if (lexer.getCurrentToken() == TokenSet.MULT_OP) {
            MATCH(thisNode, TokenSet.MULT_OP);
            FACTOR(thisNode);
            FACTOR_TAIL(thisNode);
        } else {
            EPSILON(thisNode);
        }
    }

    // <TERM_TAIL> ::= ADD_OP <TERM> <TERM_TAIL> | ε
    private void TERM_TAIL(final TreeNode parentNode) throws ParseException {
        final TreeNode thisNode = codeGenerator.addNonTerminalToTree(parentNode);

        if (lexer.getCurrentToken() == TokenSet.ADD_OP) {
            MATCH(thisNode, TokenSet.ADD_OP);
            TERM(thisNode);
            TERM_TAIL(thisNode);
        } else {
            EPSILON(thisNode);
        }
    }

    // <ARG_LIST> ::= <EXPR> <ARGS_TAIL>
    private void ARG_LIST(final TreeNode parentNode) throws ParseException {
        final TreeNode thisNode = codeGenerator.addNonTerminalToTree(parentNode);

        EXPR(thisNode);
        ARGS_TAIL(thisNode);
    }

    // <ARGS_TAIL> ::= COMMA <ARG_LIST> | ε
    private void ARGS_TAIL(final TreeNode parentNode) throws ParseException {
        final TreeNode thisNode = codeGenerator.addNonTerminalToTree(parentNode);

        if (lexer.getCurrentToken() == TokenSet.COMMA) {
            MATCH(thisNode, TokenSet.COMMA);
            ARG_LIST(thisNode);
        } else {
            EPSILON(thisNode);
        }

    }

    // <CONDITION> ::= <EXPR> REL_OP <EXPR>
    private void CONDITION(final TreeNode parentNode) throws ParseException {
        final TreeNode thisNode = codeGenerator.addNonTerminalToTree(parentNode);

        EXPR(thisNode);
        if (lexer.getCurrentToken() == TokenSet.REL_OP) {
            MATCH(thisNode, TokenSet.REL_OP);
            EXPR(thisNode);
        } else {
            var errorMessage = "SYNTAX ERROR: '%s' was found."
                    .formatted(lexer.getCurrentLexeme());
            codeGenerator.syntaxError(errorMessage, thisNode);
        }

    }

    /////////////////////////////////////////////////////////////////////////////////////

    /**
     * Add an EMPTY terminal node (result of an Epsilon Production) to the parse
     * tree.
     * Mainly, this is just done for better visualizing the complete parse tree.
     *
     * @param parentNode The parent of the terminal node.
     */
    private void EMPTY(final TreeNode parentNode) {
        codeGenerator.addEmptyToTree(parentNode);
    }

    /**
     * Match the current token with the expected token.
     * If they match, add the token to the parse tree, otherwise throw an exception.
     *
     * @param currentNode   The current terminal node.
     * @param expectedToken The token to be matched.
     * @throws ParseException Thrown if the token does not match the expected token.
     */
    private void MATCH(final TreeNode currentNode, final TokenSet expectedToken) throws ParseException {
        final var currentToken = lexer.getCurrentToken();
        final var currentLexeme = lexer.getCurrentLexeme();

        if (currentToken == expectedToken) {
            codeGenerator.addTerminalToTree(currentNode, currentToken, currentLexeme);
            lexer.advanceToken();
        } else {
            final var errorMessage = "SYNTAX ERROR: '%s' was expected\nbut '%s' was found (%s)."
                    .formatted(expectedToken, currentLexeme, currentToken);

            codeGenerator.syntaxError(errorMessage, currentNode);
        }
    }
}
