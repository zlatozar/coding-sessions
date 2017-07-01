package MapColoring.demo

import MapColoring.Country
import MapColoring.DefineCountries
import MapColoring.GeographicMaps
import MapColoring.Paint

class Main {

    public static void main(String[] args) {
        println('\n     - First we took America states map')
        def boundaries = new GeographicMaps.America().borders()

        println("\n     - Then we crate states with its properties ('color' in our case)")
        Set<Country> countryAmerica = new DefineCountries(boundaries).build()

        println('\nHere is how it looks like when we use 4 colors:\n\n')
        Paint paint = new Paint(countryAmerica).withNumberOfColors(4)
        println(paint.start())
    }
}
