package PrintersDevil

class Constants {

    final static String WORDS_SEP = ' '
    final static String LINE_SEP = System.getProperty("line.separator")

    final static Set<String> SENTENCE_END = ['.', '?', '!', '.)', '?)', '!)', '."', '?"', '!"', '.")', '?")', '!")', ':']

    final static String UNFILLED_mode = 'unfilled'  // line as is
    final static String FILL_mode     = 'fill'      // as tightly as possible (default)
    final static String JUSTIFY_mode  = 'justify'   // add extra blanks to aline last word to right margin

}
