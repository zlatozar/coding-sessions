package com.hackerrank;

import java.util.*;

public class RemoveSubStrings {

    // Algorithm

    protected String reduceString(String theString, List<String> subStr) {

        if (theString.length() < subStr.get(0).length()) {
            return theString;
        }

        // TODO

        return null;
    }

    private int getLength(String reduced) {
        return reduced.length();
    }

    // Prepare data

    protected List<String> takeArguments() {
        final Scanner scanner = new Scanner(System.in);
        final String theString = scanner.next();

        final int numSubStr = scanner.nextInt();
        final Set<String> subStr = new HashSet<String>(numSubStr);

        for (int i = 0; i < numSubStr; i++) {
            subStr.add(scanner.next());
        }

        System.out.println("string: " + theString);
        System.out.print("sub-strings: ");
        for (String sub : subStr) {
            System.out.print(sub);
            System.out.print(" ");
        }
        System.out.println();

        List<String> result = new ArrayList<>();
        result.add(theString);
        result.addAll(subStr);

        return result;
    }

    public static void main(String[] args) {
        RemoveSubStrings removeSubStr = new RemoveSubStrings();

        List<String> cmdArgs = removeSubStr.takeArguments();
        String longString = cmdArgs.get(0);
        List<String> subStr = cmdArgs.subList(1, cmdArgs.size());

        String reduced = removeSubStr.reduceString(longString, subStr);
        removeSubStr.getLength(reduced);
    }
}
