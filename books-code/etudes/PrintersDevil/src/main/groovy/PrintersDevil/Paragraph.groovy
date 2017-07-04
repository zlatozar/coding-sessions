package PrintersDevil

/**
 * Contains paragraph modes manipulation functions.
 */
class Paragraph {

    private final Environment env

    Paragraph(Environment env) {
        this.env = env
    }

    String unfilled(String rawLines) {

        String[] lines = rawLines.split(Constants.LINE_SEP)

        StringBuilder allLines = new StringBuilder()

        // first line
        for (int i = 0; i < env.getMarginLeft(); i++) {
            allLines.append(Constants.WORDS_SEP)
        }

        allLines.append(fitOrIgnore(lines[0], env.getTextWidth() - env.getMarginLeft()))
        allLines.append(Constants.LINE_SEP)

        // rest
        for (int i = 1; i < lines.size(); i++) {
            for (int j = 0; j < env.getMarginLeft(); j++) {
                allLines.append(Constants.WORDS_SEP)
            }
            allLines.append(fitOrIgnore(lines[i], env.getTextWidth()))
            allLines.append(Constants.LINE_SEP)
        }

        return allLines.toString()
    }

    /**
     * Get chunk as big as page width, ignore rest
     */
    private static String fitOrIgnore(String line, int width) {

        if (line.length() == 0) {
            return ''
        }

        if (line.length() <= width) {
            return line
        }

        return line.substring(0, width)
    }

    /**
     * Start adding word by word and when capacity of the line
     * finish add SPACE and continue.
     */
    String filled(String rawLines) {

        String[] lines = rawLines.split(Constants.LINE_SEP)
        StringBuilder allLines = new StringBuilder()

        int currentLineLength = 0
        for (int i = 0; i < lines.length; i++) {

            // add indent in first line
            if (i == 0) {
                for (int j = 0; j < env.getParagraphFirstLineStartPosition(); j++) {
                    allLines.append(Constants.WORDS_SEP)
                    currentLineLength++
                }
            }

            String[] words = lines[i].split(Constants.WORDS_SEP)

            words.each {

                if (it.length() == 0) {
                    return
                }

                if (currentLineLength < env.getTextWidth() - it.length()) {
                    allLines.append(it)
                    currentLineLength = currentLineLength + it.length()

                    if (currentLineLength + 1 < env.getTextWidth()) {
                        allLines.append(Constants.WORDS_SEP)
                        currentLineLength++
                    }

                } else {
                    allLines.append(Constants.LINE_SEP)
                    for (int j = 0; j < env.getTextStartPosition(); j++) {
                        allLines.append(Constants.WORDS_SEP)
                        currentLineLength++
                    }
                    allLines.append(it)
                    allLines.append(Constants.WORDS_SEP)
                    currentLineLength = it.length() + 1
                }
            }
        }

        return allLines.toString()
    }

    /**
     * For every line take word gaps. Add to them redundant spaces in random way.
     */
    String justify(String rawLines) {

        String filled = filled(rawLines)
        StringBuilder allLines = new StringBuilder()

        String[] lines = filled.split(Constants.LINE_SEP)

        // first
        allLines.append(fitAndJustify(lines[0], env.getPapersizeWidth(), env.getParagraphFirstLineStartPosition()))

        // rest
        for (int i = 1; i < lines.length - 1; i++) {
            allLines.append(fitAndJustify(lines[i], env.getPapersizeWidth(), env.getTextStartPosition()))
        }

        // last line as it is
        allLines.append(lines[lines.length - 1])

        return allLines.toString()
    }

    private String fitAndJustify(String rawLine, int pageWidth, int reservedSpaces) {

        StringBuilder allLines = new StringBuilder()

        for (int i = 0; i < reservedSpaces; i++) {
            allLines.append(Constants.WORDS_SEP)
        }

        // line could end with space
        int availableBlanks = pageWidth - calculateFilledLength(rawLine) - reservedSpaces

        if (availableBlanks == 0) {
            allLines = new StringBuilder()
            allLines.append(rawLine)

        } else {

            List<String> words = extractWords(rawLine)

            if (words.size() == 1) {
                allLines.append(words[0])
                return allLines
            }

            int availableGaps = words.size() - 1

            List<Integer> lengthOfGaps = []
            for (int j = 0; j < availableGaps; j++) {
                lengthOfGaps.add(1)
            }

            for (int k = 0; k < availableBlanks; k++) {
                int index = k % lengthOfGaps.size()
                lengthOfGaps[index] = lengthOfGaps[index] + 1
            }

            randomizeList(lengthOfGaps)

            for (int f = 0; f < words.size(); f++) {

                allLines.append(words[f])

                for (int h = 0; h < lengthOfGaps[f]; h++) {
                    allLines.append(Constants.WORDS_SEP)
                }

            }
        }

        allLines.append(Constants.LINE_SEP)

        return allLines.toString()
    }

    // Helper functions

    private static List<String> extractWords(String allWords) {

        List<String> words = []

        String[] split = allWords.split(Constants.WORDS_SEP)
        for (String word : split) {
            if (word.length() > 0) {
                words.add(word)
            }
        }

        return words
    }

    private static int calculateFilledLength(String allWords) {

        int lineLength = 0
        int numberOfWords = 0

        String[] split = allWords.split(Constants.WORDS_SEP)

        for (String word : split) {
            if (word.size() > 0) {
                lineLength += word.size()
                numberOfWords++
            }
        }

        return lineLength + (numberOfWords - 1)
    }

    private static void randomizeList(List<Integer> list) {
        Collections.shuffle(list)
    }
}
