package PrintersDevil

import spock.lang.Specification

class PageSpec extends Specification {

    private final static int PAGE_WIDTH = 42

    def 'Text should be displayed in a page'() {

        given: 'Page paper and paragraph'
        Environment env = new Environment()
        env.setPapersize(12, PAGE_WIDTH)
        def paragraph = new Paragraph(env)

        String text =
                'This sample section of text will be set justified. Notice that\n' +
                        '   the way    spaces are left  has  no effect\n' +
                        'on     the output.\n' +
                        'Only word separation is caused by spaces.\n' +
                        'Thus, it is a good idea to start each source text sentence on a\n' +
                        'new line to make editing easier.'

        when: 'Try to store it in a page'
        Page page = new Page(env)

        then: 'It should be widen to the end of the page line'
        String pageContent =
                '--------------------------------------------\n' +
                '|   This sample section of text will be    |\n' +
                '|set justified. Notice that the way spaces |\n' +
                '|are left has no effect on the output.     |\n' +
                '|Only word separation is caused by spaces. |\n' +
                '|Thus, it is a good idea to start each     |\n' +
                '|source text sentence on a new line to     |\n' +
                '|make editing easier.                      |\n' +
                '|                                          |\n' +
                '|                                          |\n' +
                '|                                          |\n' +
                '|                                          |\n' +
                '|                                          |\n' +
                '|                                          |\n' +
                '--------------------------------------------'

        page.display(paragraph.filled(text))
   }
}