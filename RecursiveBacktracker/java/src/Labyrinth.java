
public class Labyrinth {
	
	private int rows;
	private int columns;
	private Color[][] grid;
	
	public Labyrinth(Color[][] grid) {
		this.rows = grid.length;
		this.columns = grid[0].length;
		this.grid = grid;
	}

	public int getRows() {
		return rows;
	}

	public int getColumns() {
		return columns;
	}
	
	public Color getXYColor(int x, int y) {
		return this.grid[x][y];
	}
	
	public void setXYColor(int x, int y, Color c) {
		this.grid[x][y] = c;
	}
	
	public boolean searchPath() {
		return searchPath(0, 0);
	}
	
	private boolean searchPath(int l, int c) {
		if (l < 0 || c < 0 || l >= this.getRows() || c >= this.getColumns())	// Se as coordenadas estiverem fora dos limites
			return false;
		else if (this.getXYColor(l, c)  != Color.BACK)	// Se as coordenadas estiverem em uma barreira ou beco sem saída
			return false;
		else if (l == (this.getRows() - 1) && c == (this.getColumns() - 1)) {	// Coordenadas pertencem ao caminho e é a saída do labirinto
			this.setXYColor(l, c, Color.PATH);
			return true;
		}
		// Caso recursivo: Tentar encontrar um caminho nas coordenadas vizinhas
		this.setXYColor(l, c, Color.PATH);
		
		if (this.searchPath(l - 1, c) || this.searchPath(l + 1, c) || this.searchPath(l, c - 1) || this.searchPath(l, c + 1))
			return true;
		
		this.setXYColor(l, c, Color.TEMPORARY);	// Beco sem saída
		return false;
	}
	
	@Override
	public String toString() {
		StringBuilder str = new StringBuilder();
		str.append("[");
		
		for (int row = 0; row < this.getRows(); row++) {
			str.append(row == 0 ? "   [ " : "    [ ");
			for (int column = 0; column < this.getColumns(); column++) {
				switch (this.grid[row][column]) {
					case BACK:
						str.append("B");
						break;
					case ANORMAL:
						str.append("A");
						break;
					case TEMPORARY:
						str.append("T");
						break;
					case PATH:
						str.append("P");
						break;
					default:
						break;
				}
				if (column < this.getColumns() - 1)
					str.append(" ");
			}
			str.append(row < this.getRows() - 1 ? " ]\n" : " ]");
		}
		str.append("    ]\n");
		
		return str.toString();
	}
}

