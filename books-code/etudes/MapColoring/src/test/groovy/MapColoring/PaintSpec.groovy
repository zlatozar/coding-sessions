package MapColoring

import spock.lang.Specification
import spock.lang.Title

@Title('Paint functionality')
class PaintSpec extends Specification {

    def 'API style'() {

        given: 'Countries map that should be painted'
        def borders = new GeographicMaps.Australia().borders()

        when: 'Start implementation'
        Set<Country> countriesMap = new DefineCountries(borders)
                .withNumberOfNeighborsOrder().build()

        then: 'Painting API should be friendly'
        Paint paint = new Paint(countriesMap).withNumberOfColors(4)
        assert paint.getNumberOfColors() == 4
    }

    def 'Check if coloring works'() {

        given: 'Australia country map'
        def boundaries = new GeographicMaps.Australia().borders()
        Set<Country> countryAustralia = new DefineCountries(boundaries).build()

        when: 'Start painting'
        Paint paint = new Paint(countryAustralia).withNumberOfColors(4)

        then: 'Only 4 colors will be needed'
        assert paint.numberOfColors == 4
        assert paint.start().size() == 7
    }

}