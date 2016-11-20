import java.util.HashMap;
import java.util.PriorityQueue;


public class HuffmanUtils {

    /**
     * Creates a symbol-frequency entries from a sequence.
     *
     * @param sequence
     * @return HashMap<Character, Integer>
     */
    private static HashMap<Character, Integer> frequenciesFromSequence(String sequence) {
        HashMap<Character, Integer> charFrequencies = new HashMap<>();

        for (int i = 0; i < sequence.length(); i++) {
            char c = sequence.charAt(i);
            if (charFrequencies.containsKey(c))
                charFrequencies.replace(c, charFrequencies.get(c)+1);
            else
                charFrequencies.put(c, 1);
        }

        return charFrequencies;
    }

    /**
     * Creates a HuffmanBinaryTree florest from a symbol-frequency entries.
     *
     * @param symbolFrequencies
     * @return PriorityQueue<HuffmanBinaryTree>
     */
    private static PriorityQueue<HuffmanBinaryTree> createFlorest(HashMap<Character, Integer> symbolFrequencies) {
        PriorityQueue<HuffmanBinaryTree> pq = new PriorityQueue<>();

        for (char c : symbolFrequencies.keySet())
            pq.add(new HuffmanBinaryTree(c, symbolFrequencies.get(c)));

        return pq;
    }

    /**
     * Creates a HuffmanBinaryTree florest from a sequence.
     *
     * @param sequence
     */
    public static PriorityQueue<HuffmanBinaryTree> florestFromSequence(String sequence) {
        return createFlorest(frequenciesFromSequence(sequence));
    }

    /**
     * Encodes a sequence using Huffman Code algorithm.
     *
     * @param sequence  Sequence to encode
     * @return String   Encoded sequence
     */
    public static String encode(String sequence) {
        System.out.printf("SEQUENCE: %s\n", sequence);

        return encode(sequence, HuffmanBinaryTree.huffmanCode(florestFromSequence(sequence)));
    }

    /**
     * Encodes a sequence with it's HuffmanBinaryTree after running Huffman Code algorithm.
     *
     * @param sequence  Sequence to encode
     * @param tree      Sequence's HuffmanBinaryTree object after running Huffman Code algorithm
     * @return String   Encoded sequence
     */
    protected static String encode(String sequence, HuffmanBinaryTree tree) {
        System.out.println(tree.toString());

        return encode(sequence, tree.symbolCodes());
    }

    /**
     * Encodes a sequence with it's symbol-code entries from HuffmanBinaryTree.
     *
     * @param sequence      Sequence to encode
     * @param symbolCodes   Symbol-code entries from HuffmanBinaryTree
     * @return String       Encoded sequence
     */
    protected static String encode(String sequence, HashMap<Character, String> symbolCodes) {
        for (char c : symbolCodes.keySet())
            System.out.printf("%c : %s\n", c, symbolCodes.get(c));
        System.out.println();

        StringBuilder str = new StringBuilder();

        for (int i = 0; i < sequence.length(); i++)
            str.append(symbolCodes.get(sequence.charAt(i)));

        String encodedSequence = str.toString();
        System.out.printf("ENCODED SEQUENCE: %s\n", encodedSequence);
        return encodedSequence;
    }

    /**
     * Performs the decoding on a encoded sequence but it requires the symbol-code entries from the HuffmanBinaryTree after running the Huffman Code algorithm.
     *
     * @param encodedSequence   Encoded sequence
     * @param symbolCodes       Symbol-code entries
     * @return String           Decoded sequence
     */
    public static String decode(String encodedSequence, HashMap<Character, String> symbolCodes) {
        HashMap<String, Character> codesSymbol = invertsSymbolCodes(symbolCodes);
        StringBuilder str = new StringBuilder();
        StringBuilder currentCode = new StringBuilder();

        for (int i = 0; i < encodedSequence.length(); i++) {
            currentCode.append(encodedSequence.charAt(i));
            String code = currentCode.toString();
            if (codesSymbol.containsKey(code)) {
                str.append(codesSymbol.get(code));
                currentCode = new StringBuilder();
            }
        }

        String decodedSequence = str.toString();
        System.out.printf("DECODED SEQUENCE: %s", decodedSequence);
        return decodedSequence;
    }

    /**
     * Inverts a HashMap entries: key becomes value and value becomes key.
     *
     * @param symbolCodes
     * @return HashMap<String, Character>
     */
    private static HashMap<String, Character> invertsSymbolCodes(HashMap<Character, String> symbolCodes) {
        HashMap<String, Character> codesSymbol = new HashMap<>();

        for (char c : symbolCodes.keySet())
            codesSymbol.put(symbolCodes.get(c), c);

        return codesSymbol;
    }
}
