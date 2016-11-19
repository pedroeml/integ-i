
public class HuffmanBinaryTree {

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

        public char getCharacter() {
            return character;
        }

        public int getFrequency() {
            return frequency;
        }

        public Node getLeft() {
            return left;
        }

        public void setLeft(Node left) {
            this.left = left;
        }

        public Node getRight() {
            return right;
        }

        public void setRight(Node right) {
            this.right = right;
        }

        public boolean isLeaf() {
            return this.left == null && this.right == null;
        }

        @Override
        public int compareTo(Node o) {
            return this.frequency - o.getFrequency();
        }
    }

    public HuffmanBinaryTree(char c, int frequency) {
        this.root = new Node(c, frequency);
        this.totalFrequency = frequency;
        this.size = 1;
    }

    private Node getRoot() {
        return this.root;
    }

    public int getSize() {
        return size;
    }

    public int getTotalFrequency() {
        return totalFrequency;
    }

    public void setRootsLeftSubtree(HuffmanBinaryTree tree) {
        this.addSubtree(true, tree);
    }

    public void setRootsRightSubtree(HuffmanBinaryTree tree) {
        this.addSubtree(false, tree);
    }

    private void addSubtree(boolean isLeftSubtree, HuffmanBinaryTree tree) {
        if (isLeftSubtree)
            this.root.setLeft(tree.getRoot());
        else
            this.root.setRight(tree.getRoot());
        this.size += tree.getSize();
        this.totalFrequency += tree.getTotalFrequency();
    }



}
