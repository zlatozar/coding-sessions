package GameOfLife

class Demo {

    public static void main(String[] args) {

        Grid grid = new Grid(20, 20).withGlider()
        Simulate simulate = new Simulate(grid)

        for (int i = 0; i < 5; i++) {
            println('Here is how star ship pass through the board')

            Display.display(simulate.evolution())
        }
    }
}
