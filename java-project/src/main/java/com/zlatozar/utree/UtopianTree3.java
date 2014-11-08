package com.zlatozar.utree;

import java.util.Scanner;

/**
 * Not optimized version
 *
 * @author zlatozar@gmail.com
 */
public class UtopianTree3 {

    private static int getLengthAfterNCycles(final int cycNumber) {

        if (cycNumber == 0) {
            return 1;
        }

        return cycNumber % 2 == 0 ? getLengthAfterNCycles(cycNumber - 1) + 1 :
                getLengthAfterNCycles(cycNumber - 1) * 2;
    }

    public static void main(String[] args) {

        final Scanner scanner = new Scanner(System.in);
        final int testCases = scanner.nextInt();

        for (int i = 0; i < testCases; i++) {
            System.out.println(getLengthAfterNCycles(scanner.nextInt()));
        }
    }

}

