import java.util.*;

public class HuffmanBinaryTree implements Comparable<HuffmanBinaryTree> {

    private Node root;
    private int size;
    private int totalFrequency;

    private class Node implements Comparable<Node> {
        private char character;
        private int frequency;
        private Node left;
        private Node right;

        public Node(char character, int frequency) {
            this.character = character;
            this.frequency = frequency;
            this.left = null;
            this.right = null;
        }

        /**
         * Returns a character.
         *
         * @return char
         */
        public char getCharacter() {
            return character;
        }

        /**
         * Returns the frequency of respective character in a sequence.
         *
         * @return int
         */
        public int getFrequency() {
            return frequency;
        }

        /**
         * Returns left child Node.
         *
         * @return Node
         */
        public Node getLeft() {
            return left;
        }

        /**
         * Sets left child Node.
         *
         * @param left
         */
        public void setLeft(Node left) {
            this.left = left;
        }

        /**
         * Returns right child Node.
         *
         * @return Node
         */
        public Node getRight() {
            return right;
        }

        /**
         * Sets right child Node.
         *
         * @param right
         */
        public void setRight(Node right) {
            this.right = right;
        }

        /**
         * Returns if this Node doesn't have any child.
         *
         * @return boolean
         */
        public boolean isLeaf() {
            return this.left == null && this.right == null;
        }

        @Override
        public int compareTo(Node o) {
            return this.frequency - o.getFrequency();
        }

        @Override
        public String toString() {
            return String.format("%c:%d", this.character, this.frequency);
        }
    }

    /**
     * Creates a HuffmanBinaryTree containing only the root.
     *
     * @param c         A sequence's character
     * @param frequency Character's frequency
     */
    public HuffmanBinaryTree(char c, int frequency) {
        this.root = new Node(c, frequency);
        this.totalFrequency = frequency;
        this.size = 1;
    }

    /**
     * Creates a HuffmanBinaryTree containing a default root and two subtrees as children.
     *
     * @param treeA Root's left subtree child
     * @param treeB Root's right subtree child
     */
    private HuffmanBinaryTree(HuffmanBinaryTree treeA, HuffmanBinaryTree treeB) {
        this.root = new Node('_', 0);
        this.totalFrequency = 0;
        this.size = 1;
        if (treeA.getRoot().getCharacter() == '_' && treeA.getRoot().getFrequency() == 0) {     // This keeps merged trees at the right side of this tree
            this.setRootsLeftSubtree(treeB);
            this.setRootsRightSubtree(treeA);
        } else {
            this.setRootsLeftSubtree(treeA);
            this.setRootsRightSubtree(treeB);
        }
    }

    /**
     * Retuns the root.
     *
     * @return Node
     */
    protected Node getRoot() {
        return this.root;
    }

    /**
     * Returns the number of nodes of this HuffmanBinaryTree object.
     *
     * @return int
     */
    public int getSize() {
        return size;
    }

    /**
     * Returns the number of the total frequency of all leaf nodes.
     *
     * @return int
     */
    public int getTotalFrequency() {
        return totalFrequency;
    }

    /**
     * Sets the root's left subtree.
     *
     * @param tree
     */
    private void setRootsLeftSubtree(HuffmanBinaryTree tree) {
        this.addSubtree(true, tree);
    }

    /**
     * Sets the root's right subtree.
     *
     * @param tree
     */
    private void setRootsRightSubtree(HuffmanBinaryTree tree) {
        this.addSubtree(false, tree);
    }

    /**
     * Sets root's left or right subtree.
     *
     * @param isLeftSubtree
     * @param tree
     */
    private void addSubtree(boolean isLeftSubtree, HuffmanBinaryTree tree) {
        if (isLeftSubtree)
            this.root.setLeft(tree.getRoot());
        else
            this.root.setRight(tree.getRoot());
        this.size += tree.getSize();
        this.totalFrequency += tree.getTotalFrequency();
    }

    /**
     * Executes Huffman Code algorithm and returns a HuffmanBinaryTree.
     *
     * @param florest A florest of HuffmanBinaryTree objects
     * @return HuffmanBinaryTree
     */
    public static HuffmanBinaryTree huffmanCode(PriorityQueue<HuffmanBinaryTree> florest) {
        if (florest.size() == 1)
            return florest.poll();
        else if (florest.size() > 1) {
            HuffmanBinaryTree treeA = florest.poll();
            HuffmanBinaryTree treeB = florest.poll();
            florest.add(new HuffmanBinaryTree(treeA, treeB));
            return huffmanCode(florest);
        }
        return null;
    }

    /**
     * Returns a HashMap of codes for each symbol in this tree's leaf nodes. It's only useful after running Huffman Code algorithm.
     *
     * @return HashMap<Character, String>
     */
    public HashMap<Character, String> symbolCodes() {
        HashMap<Character, String> codes = new HashMap<>();
        storeLeafNodesCode(codes, "", this.root);
        return codes;
    }

    /**
     * Recursiverly stores leaf nodes character and code.
     *
     * @param symbolCodes   HashMap containing symbols and codes entries
     * @param prefix        Current recursion prefix
     * @param n             Current recursion node
     */
    private void storeLeafNodesCode(HashMap<Character, String> symbolCodes, String prefix, Node n) {
        if (n != null && n.isLeaf())
            symbolCodes.put(n.getCharacter(), prefix);
        else {
            storeLeafNodesCode(symbolCodes, prefix+"0", n.getLeft());
            storeLeafNodesCode(symbolCodes, prefix+"1", n.getRight());
        }
    }

    @Override
    public int compareTo(HuffmanBinaryTree o) {
        return this.totalFrequency - o.getTotalFrequency();
    }

    @Override
    public String toString() {
        return (new BinaryTreeToString(this)).toString();
    }

    /**
     * The following class was adapted from: http://stackoverflow.com/questions/4965335/how-to-print-binary-tree-diagram
     */
    private class BinaryTreeToString {

        private StringBuilder str;
        private HuffmanBinaryTree tree;

        public BinaryTreeToString(HuffmanBinaryTree tree) {
            str = new StringBuilder();
            this.tree = tree;
            this.printNode();
        }

        public <T extends Comparable<?>> void printNode() {
            int maxLevel = maxLevel(this.tree.getRoot());

            printNodeInternal(Collections.singletonList(root), 1, maxLevel);
        }

        private <T extends Comparable<?>> void printNodeInternal(List<Node> nodes, int level, int maxLevel) {
            if (nodes.isEmpty() || isAllElementsNull(nodes))
                return;

            int floor = maxLevel - level;
            int endgeLines = (int) Math.pow(2, (Math.max(floor - 1, 0)));
            int firstSpaces = (int) Math.pow(2, (floor)) - 1 + (floor != 0 ? floor - level/floor : floor - level/2);
            int betweenSpaces = (int) Math.pow(2, (floor + 1)) - 1 - Math.round(floor/2);

            printWhitespaces(firstSpaces);

            List<Node> newNodes = new ArrayList<>();
            for (Node node : nodes) {
                if (node != null) {
                    this.str.append(node.toString());
                    newNodes.add(node.getLeft());
                    newNodes.add(node.getRight());
                } else {
                    newNodes.add(null);
                    newNodes.add(null);
                    this.str.append(" ");
                }

                printWhitespaces(betweenSpaces);
            }
            this.str.append("\n");

            for (int i = 1; i <= endgeLines; i++) {
                for (int j = 0; j < nodes.size(); j++) {
                    printWhitespaces(firstSpaces - i);
                    if (nodes.get(j) == null) {
                        printWhitespaces(endgeLines + endgeLines + i + 1);
                        continue;
                    }

                    if (nodes.get(j).left != null)
                        this.str.append("/");
                    else
                        printWhitespaces(1);

                    printWhitespaces(i + i - 1);

                    if (nodes.get(j).right != null)
                        this.str.append("\\");
                    else
                        printWhitespaces(1);

                    printWhitespaces(endgeLines + endgeLines - i);
                }

                this.str.append("\n");
            }

            printNodeInternal(newNodes, level + 1, maxLevel);
        }

        private void printWhitespaces(int count) {
            for (int i = 0; i < count; i++)
                this.str.append(" ");
        }

        private <T extends Comparable<?>> int maxLevel(Node node) {
            if (node == null)
                return 0;

            return Math.max(maxLevel(node.left), maxLevel(node.right)) + 1;
        }

        private <T> boolean isAllElementsNull(List<T> list) {
            for (Object object : list)
                if (object != null)
                    return false;

            return true;
        }

        @Override
        public String toString() {
            return this.str.toString();
        }
    }
}
