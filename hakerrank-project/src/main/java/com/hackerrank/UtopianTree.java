package com.hackerrank;

import java.util.Scanner;

public class UtopianTree {

    private static int h = 0;

    private static int getLengthAfterNCycles(final int cycNumber) {

        if (cycNumber == 0) {
            return 1;
        }

        if (cycNumber % 2 == 0) {
            h = h + 1;

        } else {
            h = h * 2;
        }

        int prev = getLengthAfterNCycles(cycNumber - 1);

        if (cycNumber % 2 == 0) {
            return prev + 1;
        }

        return prev * 2;
    }

    public static void main(String[] args) {

        final Scanner scanner = new Scanner(System.in);
        final int testCases = scanner.nextInt();

        for (int i = 0; i < testCases; i++) {
            System.out.println(getLengthAfterNCycles(scanner.nextInt()));
        }
    }

}
