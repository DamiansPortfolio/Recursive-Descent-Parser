
import javax.swing.*;
import java.awt.*;
import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.List;
import java.util.Queue;
import java.util.*;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import static java.nio.file.Files.lines;
import static java.text.MessageFormat.format;

class MAIN {

    static boolean PROMPT_FOR_GRAPHVIZ;

    /**
     * The main entry point for the program.
     *
     * @param args The command line arguments.
     */
    public static void main(String[] args) {
        String fileName;

        // Check for an input file argument
        if (args.length != 1) {
            System.err.println("Must Provide an input filename!!");

            var optFileName = getFileNameFromFileChooser();

            if (optFileName.isEmpty()) {
                return; // Bailout if no file was chosen.
            } else {
                fileName = optFileName.get();
            }

        } else {
            fileName = args[0];
        }

        // Check for a valid file
        final File file = new File(fileName);

        if (!file.exists() || !file.isFile() || !file.canRead()) {
            System.err.printf("Input file not found/readable: %s%n", file.toPath());
            System.exit(1);
        }

        // Try to compile the input file.
        try {
            final String compiledCode = scanAndParse(file);

            // Display the graphviz test page, if desired.
            GraphViewer.openWebGraphViz(compiledCode, PROMPT_FOR_GRAPHVIZ);

        } catch (IOException ex) {
            final String msg = format("Error reading the file!!! {0}", ex.getMessage());
            Logger.getGlobal().log(Level.SEVERE, msg);
            System.exit(2);
        }
    }

    /**
     * Get the filename from the file chooser.
     * <p>
     * This is just a quick hack, as it will only be when the program is not invoked
     * properly anyhow.
     *
     * @return The filename from the file chooser.
     */
    private static Optional<String> getFileNameFromFileChooser() {
        try {
            UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
        } catch (Exception e) {
            return Optional.empty();
        }

        JFileChooser fileChooser = new JFileChooser();
        fileChooser.setCurrentDirectory(new File(System.getProperty("user.dir")));

        if (fileChooser.showOpenDialog(null) == JFileChooser.APPROVE_OPTION) {
            return Optional.of(fileChooser.getSelectedFile().getAbsolutePath());
        } else {
            return Optional.empty();
        }
    }

    /**
     * Scan and parse the input file.
     *
     * @param inputFile The input file to be scanned and parsed.
     * @return The compiled code.
     * @throws IOException If the file cannot be read.
     */
    private static String scanAndParse(final File inputFile) throws IOException {
        // Create the code generator and lexical analyzer.
        final CodeGenerator codeGenerator = new CodeGenerator();
        final LexicalAnalyzer lexicalAnalyzer = new LexicalAnalyzer();

        // Init the lexer
        lexicalAnalyzer.buildTokenQueue(inputFile.toPath());

        // Compile the program from the input supplied by the lexical analyzer.
        final Parser parser = new Parser(lexicalAnalyzer, codeGenerator);

        // Generate a header for the output
        TreeNode startNode = codeGenerator.writeHeader();

        // Analyze the input and generate the parse tree.
        parser.analyze(startNode);

        // generate footer for our output
        codeGenerator.writeFooter();

        // Return the compiled code.
        return codeGenerator.getGeneratedCodeBuffer();
    }
}

// *********************************************************************************************************

/**
 * This is a *FAKE* (Overly simplified) Lexical Analyzer...
 * <p>
 * NOTE: This DOES NOT "lex" the input in the traditional manner on a DFA based
 * "state machine".
 * <p>
 * Instead of using "state transitions", this is merely a quick hack to create
 * something that BEHAVES like a traditional lexer in its FUNCTIONALITY, but it
 * ONLY knows how to separate (tokenize) lexemes matching a simple regular
 * expression. A Real TokenSet would tokenize based upon far more sophisticated
 * lexical rules/expressions.
 */
class LexicalAnalyzer {
    public static final String NUMBER_REGEX = "\\d+(?:\\.\\d+)?";
    public static final String ID_REGEX = "[a-zA-Z]+\\w*";
    public static final String MULTICHAR_OPERATOR_REGEX = "<-|->|<=|=>|>=|=<|<>|:=|==|!=|&&|\\|\\|";
    public static final String OTHER_REGEX = "\\S+?";

    // Token Regex for words, numbers, and symbols.
    private final static String regex = String.join("|",
            List.of(NUMBER_REGEX, ID_REGEX, MULTICHAR_OPERATOR_REGEX, OTHER_REGEX));

    // The Compiled pattern.
    private final Pattern pattern;

    // TOKENIZED input.
    private Queue<TokenString> tokenList;

    /**
     * Construct the Lexer.
     * 
     * buildTokenQueue must be invoked to "prime" the Lexer.
     */
    LexicalAnalyzer() {
        this.pattern = Pattern.compile(regex);
    }

    /**
     * Read the file and tokenize the input.
     *
     * @param filePath the path to the file to be tokenized.
     * @throws IOException if the file cannot be read.
     */
    void buildTokenQueue(Path filePath) throws IOException {
        // Read the file and tokenize the input.
        try (var lines = lines(filePath)) {
            this.tokenize(lines // read lines
                    .map(String::trim) // map to stripped strings
                    .filter(x -> !x.startsWith("#")) // filter out lines starting with #
                    .collect(Collectors.joining(" "))); // join lines together with spaces between.
        }
    }

    /**
     * Tokenize the input string.
     *
     * @param line the input string to be tokenized.
     */
    private void tokenize(final CharSequence line) {
        // Create a matcher for the input string.
        final Matcher matcher = pattern.matcher(line);

        // Add all matches to the token list.
        this.tokenList = new LinkedList<>();

        // Add all matches to the token list.
        while (matcher.find()) {
            // Create a new token from the match.
            var lexeme = matcher.group();
            var token = TokenSet.getTokenFromLexeme(lexeme);
            TokenString tokenLexemePair = new TokenString(lexeme, token);

            // Debugging output only
            // System.out.println("READING: " + tokenLexemePair);

            // Add the token to the list.
            this.tokenList.add(tokenLexemePair);
        }

    }

    /**
     * Get the current lexeme from the head of the token list.
     *
     * @return the current lexeme.
     */
    public String getCurrentLexeme() {
        return (this.tokenList.isEmpty() || getCurrentToken() == TokenSet.$$)
                ? "$$ (End of Input)"
                : Objects.requireNonNull(this.tokenList.peek()).lexeme;
    }

    /**
     * Get the current token type from the head of the token list.
     *
     * @return the current token type.
     */
    public TokenSet getCurrentToken() {
        return this.tokenList.isEmpty()
                ? TokenSet.$$
                : this.tokenList.peek().token;
    }

    /**
     * Get the current token type from the head of the token list.
     */
    public void advanceToken() {
        if (!this.tokenList.isEmpty()) {
            this.tokenList.remove();
        }
    }

    @Override
    public String toString() {
        return this.tokenList.toString();
    }

    /**
     * Nested class: a "Pair Tuple/Struct" for the token type and original string.
     */
    private record TokenString(String lexeme, TokenSet token) {
        @Override
        public String toString() {
            return String.format("{lexeme='%s', token=%s}", lexeme, token);
        }
    }
}

// *********************************************************************************************************

/**
 * This is a ***SIMULATION*** of a "code generator" that simply generates
 * GraphViz output.
 * Technically, this would represent the "Intermediate Code Generation" step.
 * <p>
 * Also, Instead of building an entire tree in memory followed by a traversal
 * tree at the end,
 * here we are just adding “code” as we go.
 * <p>
 * (This simulates a single-pass compiler; keep in mind that most modern
 * compilers work in several
 * passes… eg. Scan for all top level identifiers, build subtrees for each
 * class/method/etc.,
 * generate an internal intermediate code representation, and so on).
 * <p>
 * DESIGN NOTE: From an OOP design perspective, creating instances of "utility
 * classes" (classes
 * with no internal state) is generally bad. However, in a more elaborate
 * example, the code
 * generator would most certainly maintain some internal state information.
 * (Memory address offsets,
 * etc.)
 */
class CodeGenerator {
    private static final String GRAPHVIZ_ROOT_STYLE = "shape=plaintext";
    private static final String GRAPHVIZ_TERMINAL_STYLE = "shape=oval, style=bold";
    private static final String GRAPHVIZ_NON_TERMINAL_STYLE = "shape=rect, style=dotted";
    private static final String GRAPHVIZ_EPSILON_STYLE = "shape=plaintext";
    private static final String GRAPHVIZ_SYNTAX_ERROR = "shape=plaintext, color=red";

    // Buffer for generated code
    private final StringBuffer generatedCodeBuffer;

    // Constructor
    CodeGenerator() {
        this.generatedCodeBuffer = new StringBuffer();
    }

    /**
     * Add an "inner node" to the parse tree.
     * <p>
     * The following code employs a bit of a trick to automatically build the
     * calling method name.
     * The "getStackTrace()" method returns information about the entire active
     * stack. Element[0] is
     * the actual "getStackTrace()" method (it does not eliminate itself from the
     * array), element[1]
     * is THIS method (since we called "getStackTrace()") and element[2] is the
     * method that called
     * us, etc.
     *
     * @param parentNode the parent of the node being added to the tree
     * @return the newly added node as ParseNode object.
     */
    TreeNode addNonTerminalToTree(final TreeNode parentNode) {
        // This uses a Java "Trick" to return the name of the function that called this
        // method.
        final var fromMethodName = Thread
                .currentThread()
                .getStackTrace()[2]
                .getMethodName()
                .toUpperCase();

        // Build a node name
        final var toNode = this.buildNode("<" + fromMethodName + ">");

        this.addNonTerminalToTree(parentNode, toNode);

        return toNode;
    }

    // Build a node name, so it can be later "deconstructed" for the output.
    private TreeNode buildNode(final String name) {
        return new TreeNode(name);
    }

    /**
     * Show the non-terminals as boxes…
     *
     * @param fromNode the parent node
     * @param toNode   the child node
     * @return the child node
     */
    private TreeNode addNonTerminalToTree(final TreeNode fromNode, final TreeNode toNode) {
        final var msg = String.format("\t\"%s\" -> {\"%s\" [label=\"%s\", %s]};%n", fromNode, toNode,
                toNode.getNodeName(),
                GRAPHVIZ_NON_TERMINAL_STYLE);

        this.outputGeneratedCode(msg);
        return toNode;
    }

    // Write generated code to both the screen AND the buffer.
    private void outputGeneratedCode(final String msg) {
        System.out.print(msg);
        this.generatedCodeBuffer.append(msg);
    }

    /**
     * Add a terminal node to the parse tree.
     *
     * @param parentNode    The parent of the terminal node.
     * @param currentToken  The token to be added.
     * @param currentLexeme The lexeme of the token beign added.
     * @throws ParseException Throws a ParseException if the token cannot be added
     *                        to the tree.
     */
    void addTerminalToTree(final TreeNode parentNode, final TokenSet currentToken,
            final String currentLexeme) throws ParseException {
        var nodeLabel = "<%s>".formatted(currentToken);
        var terminalNode = addNonTerminalToTree(parentNode, nodeLabel);

        addTerminalToTree(terminalNode, currentLexeme);
    }

    /**
     * Add the "from node" to the tree and return a new "next node" object.
     *
     * @param fromNode The node to add to the tree.
     * @return the newly added node as ParseNode object.
     */
    TreeNode addNonTerminalToTree(final TreeNode fromNode, final String toNodeString) {
        final var toNode = this.buildNode(toNodeString);
        return this.addNonTerminalToTree(fromNode, toNode);
    }

    /**
     * Add a terminal node to the parse tree.
     *
     * @param fromNode The parent of the terminal node.
     * @param lexeme   The lexeme of the token to be added.
     */
    void addTerminalToTree(final TreeNode fromNode, final String lexeme) {
        final var node = new TreeNode(lexeme);
        String template = "\t\"%s\" -> {\"%s\" [label=\"%s\", %s]};%n";
        final var msg = String.format(template, fromNode, node, lexeme, GRAPHVIZ_TERMINAL_STYLE);

        this.outputGeneratedCode(msg);
    }

    /**
     * Add an EMPTY terminal node (result of an Epsilon Production) to the parse
     * tree.
     * Mainly, this is just done for better visualizing the complete parse tree.
     *
     * @param fromNode The parent of the terminal node.
     */
    void addEmptyToTree(final TreeNode fromNode) {
        final var node = new TreeNode("EMPTY");
        String template = "\t\"%s\" -> {\"%s\" [label=\"%s\", %s]};%n";
        final var msg = String.format(template, fromNode, node, "&epsilon;", GRAPHVIZ_EPSILON_STYLE);

        this.outputGeneratedCode(msg);
    }

    // Call this if a syntax error occurs…

    /**
     * Output the error message and throw a ParseException.
     *
     * @param err      The error message to be displayed.
     * @param fromNode The node from which the error occurred.
     * @throws ParseException Thrown with the error message.
     */
    void syntaxError(final String err, TreeNode fromNode) throws ParseException {
        String template = "\t\"%s\" -> {\"%s\" [%s]};%n}%n";
        var msg = String.format(template, fromNode, err, GRAPHVIZ_SYNTAX_ERROR);

        this.outputGeneratedCode(msg);
        throw new ParseException(err);
    }

    /**
     * Write the header for the "compiled" output.
     *
     * @return The newly created node.
     */
    TreeNode writeHeader() {
        // The header for the "compiled" output
        var headerNode = this.buildNode("Parse Tree");

        var template = "digraph ParseTree {%n" + "\t\"%s\" [label=\"%s\", %s];%n";
        var msg = String.format(template, headerNode, headerNode.getNodeName(), GRAPHVIZ_ROOT_STYLE);
        this.outputGeneratedCode(msg);

        return headerNode;
    }

    /**
     * Write the footer for the "compiled" output.
     * "Real" executable code generally has a footer.
     * See: <a href=
     * "https://en.wikipedia.org/wiki/Executable_and_Linkable_Format">...</a>
     */
    void writeFooter() {
        final var msg = "}\n";
        this.outputGeneratedCode(msg);
    }

    /**
     * Get the generated code buffer.
     *
     * @return The generated code buffer.
     */
    String getGeneratedCodeBuffer() {
        return generatedCodeBuffer.toString();
    }
}

// *********************************************************************************************************

/**
 * A "3-Tuple" for the node name and ID number.
 */
class TreeNode {

    private static Integer runningNodeID = 0;
    private final String nodeName;
    private final Integer nodeId;

    TreeNode(final String nodeName) {
        this.nodeName = nodeName;

        // Note that assignment to static fields is generally a bad practice, but it is
        // done here only for simplicity.
        this.nodeId = TreeNode.runningNodeID++;
    }

    /**
     * Getters for the node name and ID.
     */
    String getNodeName() {
        return nodeName;
    }

    /**
     * Get the node ID.
     *
     * @return The node ID.
     */
    private Integer getNodeId() {
        return nodeId;
    }

    /**
     * Convert the node to a string.
     */
    @Override
    public String toString() {
        return String.format("%s-%s", this.getNodeName(), this.getNodeId());
    }
}

// *********************************************************************************************************
// Rev Hash: APQMD6dwP2WYeh65
// *********************************************************************************************************

/**
 * Code to invoke the online graph viewer.
 */
class GraphViewer {

    /**
     * To open a browser window…
     */
    static void openWebGraphViz(final String graph, boolean promptForGraphviz) {
        /*
         * Online/Web versions of Graphviz
         * http://www.webgraphviz.com
         * http://viz-js.com
         * https://dreampuf.github.io/GraphvizOnline
         */
        final var WEBGRAPHVIZ_HOME = "https://dreampuf.github.io/GraphvizOnline/";
        final var GraphvizInfo = """
                To visualize your output, you may Copy/Paste the parser output into:
                %s
                (or any locally installed or online Graphviz rendering tool: https://graphviz.org)

                To enable/disable automatic rendering, add the following in your Parser class constructor.
                MAIN.PROMPT_FOR_GRAPHVIZ = (true|false);
                """.formatted(WEBGRAPHVIZ_HOME);

        // For some reason, the URL encoder always uses "+" instead of "%20" for spaces,
        // which are not always accepted.
        // So, replace the "+" with "%20" to be sure.
        String encodedURL = URLEncoder.encode(graph, StandardCharsets.UTF_8).replace("+", "%20");

        // URI Length limit reached.
        if (WEBGRAPHVIZ_HOME.length() + encodedURL.length() >= 32_000) {
            System.out.println("Sorry, can't use remote Graphviz; Output is too long.");
            System.out.println(GraphvizInfo);
            return;
        }

        // Can we open a browser?
        if (promptForGraphviz && Desktop.isDesktopSupported()) {
            // Try to set the default L&F to the system L&F.
            try {
                UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
            } catch (ClassNotFoundException | InstantiationException | IllegalAccessException
                    | UnsupportedLookAndFeelException e) {
                // Use the default look and feel instead, whatever it may be. (Though, it
                // *should* be the system L&F.)
            }

            // Open the default browser with the url:
            try {
                // Create a URI from the encoded URL.
                final URI webGraphvizURI = new URI(WEBGRAPHVIZ_HOME + "#" + encodedURL);

                // Get the desktop object.
                final Desktop desktop = Desktop.getDesktop();

                // Can we launch a browser?
                if (desktop.isSupported(Desktop.Action.BROWSE)) {
                    String dialogMsg = "<html><b>Open a web-based Graphviz instance?</b><br>" +
                            "(" + WEBGRAPHVIZ_HOME + ")<br><br>" +
                            "<font size='1'>To disable this prompt, add the following in your Parser class constructor:<br>"
                            +
                            "<code>MAIN.PROMPT_FOR_GRAPHVIZ = false</code></font><br><br></html>";

                    final var response = JOptionPane.showConfirmDialog(
                            null,
                            String.format(dialogMsg, WEBGRAPHVIZ_HOME), "Open Web-Based Graphviz?",
                            JOptionPane.YES_NO_OPTION);

                    // Open Browser?
                    if (response == JOptionPane.YES_OPTION) {
                        desktop.browse(webGraphvizURI);
                    } else {
                        System.out.println(GraphvizInfo);
                    }
                }
            } catch (IOException | URISyntaxException ex) {
                Logger logger = Logger.getAnonymousLogger();
                logger.log(java.util.logging.Level.WARNING, "Could not open a browser.", ex);
            }
        }
    }
}

// *********************************************************************************************************

/**
 * An exception to be raised if parsing fails due to a "syntax error" in the
 * input file.
 */
final class ParseException extends RuntimeException {
    /**
     * Create a new ParseException with the given error message.
     *
     * @param errMsg The error message to be displayed.
     */
    ParseException(String errMsg) {
        super(errMsg);
    }
}
