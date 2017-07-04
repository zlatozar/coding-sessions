package PrintersDevil

import spock.lang.Specification
import spock.lang.Title

@Title('Checks how well interpretation works')
class InterpretCmdSpec extends Specification {

    private InterpretLine interpretLine = new InterpretLine()

    def '?papersize [height width] (default 40 72) (break: TRUE)'() {

        given: 'Correct ?papersize command'
        String papersise = '?papersize 50 80 4'

        when: 'It is interpreted'
        interpretLine.getEnvironment().getPapersizeHeight() == 40
        interpretLine.getEnvironment().getPapersizeWidth() == 72
        interpretLine.process(papersise)

        then: 'Parameters should be set'
        assert interpretLine.getEnvironment().getPapersizeHeight() == 50
        assert interpretLine.getEnvironment().getPapersizeWidth() == 80
    }

    def 'Check ?mode [unfilled|fill|justify]'() {

        given: 'Correct ?mode command'
        String mode = '?mode justify'

        when: 'It is interpreted'
        interpretLine.process(mode)

        then: 'Parameters should be set'
        assert interpretLine.getEnvironment().getParagraphMode() == 'justify'
    }

    def '?paragraph [indent gap] (default 3 0) (break: FALSE)'() {

        given: 'Correct ?paragraph command'
        String paragraph = '?paragraph 4 2'

        when: 'It is interpreted'
        interpretLine.process(paragraph)

        then: 'Parameters should be set'
        assert interpretLine.getEnvironment().getParagraphIndent() == 4
        assert interpretLine.getEnvironment().getParagraphGap() == 2
    }

    def '?margin [left right] (break: TRUE)'() {

        given: 'Correct ?margin command'
        String margin = '?margin 10 20'

        when: 'It is interpreted'
        interpretLine.process(margin)

        then: '$margin should be set'
        assert interpretLine.getEnvironment().getMarginLeft() == 10
        assert interpretLine.getEnvironment().getMarginRight() == 20
        assert interpretLine.getEnvironment().breakParagraph()
    }

    def '?linespacing [gap] (default 1) (break: TRUE)'() {

        given: 'Correct ?linespacing command'
        String linespacing = '?linespacing 10 20'

        when: 'It is interpreted'
        interpretLine.process(linespacing)

        then: 'Parameters should be set'
        assert interpretLine.getEnvironment().getLinespacingGap() == 10
        assert interpretLine.getEnvironment().breakParagraph()
    }

    def '?space [n] (default 0) (break: TRUE)'() {

        given: 'Correct ?space command'
        String space = '?space 10'

        when: 'It is interpreted'
        interpretLine.process(space)

        then: 'Parameters should be set'
        assert interpretLine.getEnvironment().getSpace_N() == 10
        assert interpretLine.getEnvironment().breakParagraph()
    }

    def '?blank [n] (break: FALSE)'() {

        given: 'Correct ?blank command'
        String blank = '?blank 10'

        when: 'It is interpreted'
        interpretLine.process(blank)

        then: 'Parameters should be set'
        assert interpretLine.getEnvironment().getBlank_N() == 10
        assert !interpretLine.getEnvironment().breakParagraph()
    }

    def '?center (break: FALSE)'() {

        given: 'Correct ?center command'
        String center = '?center 10'

        when: 'It is interpreted'
        interpretLine.process(center)

        then: 'Parameters should be set'
        assert interpretLine.getEnvironment().toCenter()
        assert !interpretLine.getEnvironment().breakParagraph()
    }

    def '?page  (break: TRUE)'() {

        given: 'Correct ?page command'
        String page = '?page 10'

        when: 'It is interpreted'
        interpretLine.process(page)

        then: 'Parameters should be set'
        assert interpretLine.getEnvironment().toNewPage()
        assert interpretLine.getEnvironment().breakParagraph()
    }

    def '?testpage [n] (break: TRUE)'() {

        given: 'Correct ?testpage command'
        String testpage = '?testpage 10'

        when: 'It is interpreted'
        interpretLine.process(testpage)

        then: 'Parameters should be set'
        assert interpretLine.getEnvironment().getTestpage_N() == 10
        assert interpretLine.getEnvironment().breakParagraph()
    }

    def '?heading [depth place position] (break: FALSE)'() {

        given: 'Correct ?heading command'
        String heading = '?heading 10 center 4 additional'

        when: 'It is interpreted'
        interpretLine.process(heading)

        then: 'Parameters should be set'
        assert interpretLine.getEnvironment().getHeadingDepth() == 10
        assert interpretLine.getEnvironment().getHeadingPlace() == 'center'
        assert interpretLine.getEnvironment().getHeadingPosition() == 4

        assert !interpretLine.getEnvironment().breakParagraph()
    }

    def '?number [n] (break: FALSE)'() {

        given: 'Correct ?number command'
        String number = '?number 10'

        when: 'It is interpreted'
        interpretLine.process(number)

        then: 'Parameters should be set'
        assert interpretLine.getEnvironment().getPageNumber() == 10
        assert !interpretLine.getEnvironment().breakParagraph()
    }

    def '?break (break: TRUE)'() {

        given: 'Correct ?break command'
        String breakCMD = '?break 10'

        when: 'It is interpreted'
        interpretLine.process(breakCMD)

        then: 'Parameters should be set'
        assert interpretLine.getEnvironment().breakParagraph()
    }

    def '?footnote [depth] (break: FALSE)'() {

        given: 'Correct ?footnote command'
        String footnote = '?footnote 10'

        when: 'It is interpreted'
        interpretLine.process(footnote)

        then: 'Parameters should be set'
        assert interpretLine.getEnvironment().getFootnoteDepth() == 10
    }

    def '?alias [fake real] (break: FALSE)'() {

        given: 'Correct ?alias command'
        String alias = '?alias fake real'

        when: 'It is interpreted'
        interpretLine.process(alias)

        then: 'Parameters should be set'
        assert interpretLine.getEnvironment().getAliasFor('real') == 'fake'
    }
}