import java.util.HashMap;

public class Main {

    public static void main(String[] args) {
        String sequence = "TTAGAACCTCTTT";
        HuffmanBinaryTree tree = HuffmanBinaryTree.huffmanCode(HuffmanUtils.florestFromSequence(sequence));
        HashMap<Character, String> symbolCodes = tree.symbolCodes();

        String encodedSequence = HuffmanUtils.encode(sequence);
        String decodedSequence = HuffmanUtils.decode(encodedSequence, symbolCodes);
    }
}
