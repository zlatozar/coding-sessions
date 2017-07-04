package PrintersDevil

import spock.lang.Specification
import spock.lang.Title

@Title('Line manipulation')
class ParagraphSpec extends Specification {

    private final static int PAGE_WIDTH = 42

    def 'Print the paragraph as it is'() {

        given: 'Some pre-formatted material'
        Environment env = new Environment()
        env.setPapersize(72, PAGE_WIDTH)
        def paragraph = new Paragraph(env)

        String unfilled =
                'This text   will be taken exactly  as\n' +
                '   seen and it   better  not  run\n' +
                'past      column    40.\n'


        when: 'Try to store it in a page as it is'

        then: 'Text should be the same'
        assert paragraph.unfilled(unfilled) == unfilled

        and: 'Longer lines are ignored after page length end'
        String unfilledLonger =
                'This text   will be taken exactly  as\n' +
                '   seen and it   better  not  run\n' +
                'past      column    40.\n' +
                '   Lastly add    line that is longer    than 40 symbols and see if will   be divided correctly. Is it?\n'

        assert paragraph.unfilled(unfilledLonger) ==
                'This text   will be taken exactly  as\n' +
                '   seen and it   better  not  run\n' +
                'past      column    40.\n' +
                '   Lastly add    line that is longer    th\n'

        and: 'Indents should NOT be made'
        env.setParagraph(3, 0)
        assert paragraph.unfilled(unfilled) ==
                'This text   will be taken exactly  as\n' +
                '   seen and it   better  not  run\n' +
                'past      column    40.\n'

        and: 'Only left margin matters'
        env.setMargin(3, PAGE_WIDTH)
        assert paragraph.unfilled(unfilled) ==
                '   This text   will be taken exactly  a\n' +
                '      seen and it   better  not  run\n' +
                '   past      column    40.\n'
    }

    def 'Print the paragraph and try to fill it'() {

        given: 'Lines are wider than page length and could contain a lot of gaps'
        Environment env = new Environment()
        env.setPapersize(72, PAGE_WIDTH)
        def paragraph = new Paragraph(env)

        String filled =
                'In the fill mode,  spaces    still have no effect,\n' +
                'but now the words are all run close up and the right margin is\n' +
                'raggedy.\n' +
                'Research suggests that the ragged right  edge\n' +
                'may improve reading speed.\n' +
                'Notice also the paragraph break caused by ?mode.\n'

        when: 'Try to store it in a page as fill and tight as possible'
        env.setParagraph(0, 0)

        then: 'It should be divided'
        String filledResult =
                'In the fill mode, spaces still have no \n' +
                'effect, but now the words are all run \n' +
                'close up and the right margin is raggedy.\n' +
                'Research suggests that the ragged right \n' +
                'edge may improve reading speed. Notice \n' +
                'also the paragraph break caused by ?mode.'

        assert paragraph.filled(filled) == filledResult

        and: 'Indents should be added'
        env.setParagraph(5, 0)
        assert paragraph.filled(filled) ==
                '     In the fill mode, spaces still have \n' +
                'no effect, but now the words are all run \n' +
                'close up and the right margin is raggedy.\n' +
                'Research suggests that the ragged right \n' +
                'edge may improve reading speed. Notice \n' +
                'also the paragraph break caused by ?mode.'

        and: 'Margin matters'
        env.setParagraph(5, 0)
        env.setMargin(3, PAGE_WIDTH)
        assert paragraph.filled(filled) ==
                '        In the fill mode, spaces still\n' +
                '   have no effect, but now the words are \n' +
                '   all run close up and the right margin \n' +
                '   is raggedy. Research suggests that the\n' +
                '   ragged right edge may improve reading \n' +
                '   speed. Notice also the paragraph break\n' +
                '   caused by ?mode. '
    }

    def 'Wide sentence to fit and justified'() {

        given: 'Line bigger than page length'
        Environment env = new Environment()
        env.setPapersize(72, PAGE_WIDTH)
        def paragraph = new Paragraph(env)

        String justify =
                'This sample section of text will be set justified. Notice that\n' +
                '   the way    spaces are left  has  no effect\n' +
                'on     the output.\n' +
                'Only word separation is caused by spaces.\n' +
                'Thus, it is a good idea to start each source text sentence on a\n' +
                'new line to make editing easier.'

        when: 'Try to store it in a page'

        then: 'It should be wide up and right aliened to the end of the page line'
        String justifiedResult =
                'This sample section of text will be set\n' +
                'justified. Notice that the way spaces are\n' +
                'left has no effect on the output. Only\n' +
                'word separation is caused by spaces. Thus,\n' +
                'it is a good idea to start each source\n' +
                'text sentence on a new line to make\n' +
                'editing easier.'

        // gaps length is in random way so can't compare
        println()
        println paragraph.justify(justify)
        println()

        and: 'Indents should be added'
        String justifiedAndIndentResult =
                '     This sample section  of  text will be\n' +
                'set justified. Notice that the way  spaces\n' +
                'are left has no effect on the output. Only\n' +
                'word separation is caused by spaces. Thus,\n' +
                'it  is  a good idea  to  start each source\n' +
                'text  sentence  on  a  new  line  to  make\n' +
                'editing easier. '

        env.setParagraph(5, 0)
        println paragraph.justify(justify)
        println()


        and: 'Margin matters'
        env.setMargin(3, PAGE_WIDTH)

        println paragraph.justify(justify)
    }
}