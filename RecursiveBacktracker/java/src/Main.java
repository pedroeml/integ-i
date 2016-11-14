
public class Main {

	public static void main(String[] args) {
		Color[][] grid = {{Color.BACK, Color.ANORMAL, Color.BACK, Color.ANORMAL, Color.ANORMAL},
	             {Color.BACK, Color.ANORMAL, Color.BACK, Color.ANORMAL, Color.ANORMAL},
	             {Color.BACK, Color.BACK,   Color.BACK, Color.ANORMAL, Color.BACK},
	             {Color.BACK, Color.ANORMAL, Color.BACK, Color.BACK,   Color.BACK},
	             {Color.BACK, Color.ANORMAL, Color.BACK, Color.ANORMAL, Color.BACK}};

		Labyrinth labyrinth = new Labyrinth(grid);
		System.out.println(labyrinth.searchPath() ? "PATH FOUND!" : "PATH NOT FOUND!");
		System.out.println(labyrinth);
	}

}
