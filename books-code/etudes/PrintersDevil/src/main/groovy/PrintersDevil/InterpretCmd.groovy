package PrintersDevil

class InterpretCmd {

    private final Environment env

    InterpretCmd(Environment env) {
        this.env = env
    }

    /**
     * Defines page size. Lines should obey:
     *
     * $PAPERSIZE WIDTH (consider $MARGIN right) <= ($MARGIN left) + ($PARAGRAPH indent) + $BLANK + line
     * $PAPERSIZE HEIGHT <= ($HEADING depth) + $PARAGRAPH(lines) + ($FOOTNOTE depth)
     */
    void $PAPERSIZE(String[] command) {

        String commandName = command[0]

        switch (command.length) {
            case 1:
            case 2:
                sendError("Only one parameter is found. $commandName accepts two parameters: 'height' and 'width' as integers")
                break

            default:
                if (command.length > 3) {
                    sendWarn("Only '${command[1]}' and '${command[2]} will be used. ${command[3..-1]} are ignored")
                }

                int height = castToInteger(command[0], command[1])
                int width = castToInteger(command[0], command[2])

                env.setPapersize(height, width)
                env.setParagraphBreak(true)
        }
    }

    /**
     * Defines how $PARAGRAPH should be displayed
     */
    void $MODE(String[] command) {

        String commandName = command[0]

        Set<String> possibleModes = [env.FILL_mode, env.JUSTIFY_mode, env.UNFILLED_mode]

        switch (command.length) {
            case 1:
                sendError("No parameters. $commandName accepts one parameter with possible values: $possibleModes")
                break

            default:
                if (command.length > 2) {
                    sendWarn("Only '${command[1]}' will be used. ${command[2..-1]} are ignored")
                }

                String mode = command[1]

                if (!possibleModes.contains(mode)) {
                    sendError("Possible values of '$commandName' command are $possibleModes")
                }

                env.setParagraphMode(mode)
        }
    }

    /**
     * Defines group of lines and $MODE defines how it should looks like
     */
    void $PARAGRAPH(String[] command) {

        String commandName = command[0]

        switch (command.length) {
            case 1:
            case 2:
                sendError("Only one parameter is found. $commandName accepts two parameters: 'indent' and 'gap' as integers")
                break

            default:
                if (command.length > 3) {
                    sendWarn("Only '${command[1]}' and '${command[2]} will be used. ${command[3..-1]} are ignored")
                }
                int indent = castToInteger(command[0], command[1])
                int gap = castToInteger(command[0], command[2])

                env.setParagraph(indent, gap)
        }
    }

    /**
     * Defines from outdent from paper size beginning
     */
    void $MARGIN(String[] command) {

        final int marginWindowWidth = 5
        String commandName = command[0]

        switch (command.length) {
            case 1:
            case 2:
                sendError("Only one parameter is found. $commandName accepts two parameters: 'left' and 'right' as intergers")
                break

            default:
                if (command.length > 3) {
                    sendWarn("Only '${command[1]}' and '${command[2]} will be used. ${command[3..-1]} are ignored")
                }

                int left = castToInteger(command[0], command[1])
                int right = castToInteger(command[0], command[2])

                if (left >= right) {
                    sendError("Left margin is bigger than right: $left > $right")
                }

                if ((right - left) < marginWindowWidth) {
                    sendError("Left and right margins are too close. They should be at least $marginWindowWidth points form each other")
                }

                int marginLeft = validateInteger(left, 1, env.getPapersizeWidth()) ?: left
                int marginRight = validateInteger(right, 5, env.getPapersizeWidth() + 1) ?: right

                env.setMargin(marginLeft, marginRight)
                env.setParagraphBreak(true)
        }
    }

    /**
     * Defines how many empty lines should be added between text lines
     */
    void $LINESPACING(String[] command) {

        String commandName = command[0]

        switch (command.length) {
            case 1:
                sendError("No parameters. $commandName accepts one parameter: 'gap' as interger")
                break

            default:
                if (command.length > 2) {
                    sendWarn("Only '${command[1]}' will be used. ${command[2..-1]} are ignored")
                }

                int gap = castToInteger(command[0], command[1])

                env.setLinespacingGap(gap)
                env.setParagraphBreak(true)
        }
    }

    /**
     * Defines how many empty lines should be added in text.
     * It will be invalidated after next line is placed
     */
    void $SPACE(String[] command) {
        String commandName = command[0]

        switch (command.length) {
            case 1:
                sendError("No parameters. $commandName accepts one parameter: 'n' as interger")
                break

            default:
                if (command.length > 2) {
                    sendWarn("Only '${command[1]}' will be used. ${command[2..-1]} are ignored")
                }

                int n = castToInteger(command[0], command[1])

                env.setSpace_N(n)
                env.setParagraphBreak(true)
        }
    }

    /**
     * Defines how many spaces should be added at the beginning of the line
     */
    void $BLANK(String[] command) {
        String commandName = command[0]

        switch (command.length) {
            case 1:
                sendError("No parameters. $commandName accepts one parameter: 'n' as interger")
                break

            default:
                if (command.length > 2) {
                    sendWarn("Only '${command[1]}' will be used. ${command[2..-1]} are ignored")
                }

                int n = castToInteger(command[0], command[1])

                env.setBlank_N(n)
        }
    }

    /**
     * Place next trimmed line in center if possible (left and right margin could be too tight)
     * Invalidate after line is placed or send error if imposable
     */
    void $CENTER(String[] command) {
        env.setCenter(true)

        if (command.length > 1) {
            sendWarn("Command '${command[0]}' do not take parametes. ${command[1..-1]} are ignored")
        }
    }

    /**
     * Explicitly defines the beginning of new page
     */
    void $PAGE(String[] command) {
        env.setNewPage(true)
        env.setParagraphBreak(true)

        if (command.length > 1) {
            sendWarn("Command '${command[0]}' do not take parametes. ${command[1..-1]} are ignored")
        }
    }

    /**
     * Checks the remaining lines on a page and if they are fewer than N
     * it works as $PAGE otherwise is completely ignored
     */
    void $TESTPAGE(String[] command) {
        String commandName = command[0]

        switch (command.length) {
            case 1:
                sendError("No parameters. $commandName accepts one parameter: 'n' as interger")
                break

            default:
                if (command.length > 2) {
                    sendWarn("Only '${command[1]}' will be used. ${command[2..-1]} are ignored")
                }

                int n = castToInteger(command[0], command[1])

                env.setTestpage_N(n)
                env.setParagraphBreak(true)
        }
    }

    /**
     * Defines how top of the page should look like.
     * Once defined it will be valid for all pages.
     *
     * Note that heading must be invalidated if ?HEADING with depth 0 is met
     */
    void $HEADING(String[] command) {

        final String[] possiblePlaces = ['left', 'center', 'right']

        String commandName = command[0]

        switch (command.length) {
            case 1:
            case 2:
            case 3:
                sendError("$commandName accepts three parameters: 'depth' (interger), 'place' ($possiblePlaces) and 'position' (integer)")
                break

            default:
                if (command.length > 4) {
                    sendWarn("Only '${command[1]}', '${command[2]}' and '${command[3]} will be used. ${command[4..-1]} are ignored")
                }

                int depth = castToInteger(command[0], command[1])
                String place = command[2]
                int position = castToInteger(command[0], command[3])

                if (position > depth) {
                    sendError("Can't reach position $position in heading with depth $depth")
                }

                if (!possiblePlaces.contains(place)) {
                    sendError("Position value could be one of $possiblePlaces")
                }

                // heading should be removed if depth is 0
                env.setHeading(depth, place, position)
        }
    }

    /**
     * Force page number changing
     */
    void $NUMBER(String[] command) {
        String commandName = command[0]

        switch (command.length) {
            case 1:
                sendError("No parameters. $commandName accepts one parameter: 'n' as interger")
                break

            default:
                if (command.length > 2) {
                    sendWarn("Only '${command[1]}' will be used. ${command[2..-1]} are ignored")
                }

                int n = castToInteger(command[0], command[1])

                env.setPageNumber(n)
        }
    }

    /**
     * Force finish of the current paragraph (cause break)
     */
    void $BREAK(String[] command) {

        if (command.length > 1) {
            sendWarn("${command[0]} do not accept parameters. ${command[1..-1]} are ignored")
        }

        env.setParagraphBreak(true)
    }

    /**
     * Defines footer as taking following N lines.
     * Note that following lines could contain commands that should be interpreted too.
     */
    void $FOOTNOTE(String[] command) {
        String commandName = command[0]

        switch (command.length) {
            case 1:
                sendError("No parameters. $commandName accepts one parameter: 'n' as interger")
                break

            default:
                if (command.length > 2) {
                    sendWarn("Only '${command[1]}' will be used. ${command[2..-1]} are ignored")
                }

                int depth = castToInteger(command[0], command[1])

                env.setFootnoteDepth(depth)
        }
    }

    /**
     * Kind of macro.
     *
     * Defines character alias that should be used instead of the real one.
     * Note that all aliases are invalidated if $ALIAS without parameters is met.
     */
    void $ALIAS(String[] command) {
        String commandName = command[0]

        switch (command.length) {
            case 1:
                // reset all aliases
                env.resetAliases()
                break

            case 2:
                sendWarn("Only one parameter is found. $commandName accepts that as ' ' (space) replacement")
                env.setAlias(' ', command[1])
                break

            default:
                if (command.length > 3) {
                    sendWarn("Only '${command[1]}' and '${command[2]} will be used. ${command[3..-1]} are ignored")
                }

                env.setAlias(command[2], command[1])
        }
    }

    // Helper methods

    private static int castToInteger(String commandName, String param) {

        try {
            return param.toInteger()

        } catch (Exception _) {
            sendError("$commandName accepts as parameters integers")
        }
    }

    private static void sendError(String message) {
        println("ERROR: $message")
        throw new RuntimeException(message)
    }

    private static void sendWarn(String message) {
        println("WARN: $message")
    }

    /**
     * Not inclusive to max value
     */
    private static void validateInteger(int value, int min, int max) {

        if (max <= min) {
            sendError("Invalid validaton min:$min > max:$max")
        }

        if (value < min && value >= max ) {
            sendError("$value is not in allowed range min:$min/max:$max")
        }
    }
}
