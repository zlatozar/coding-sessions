package com.zlatozar.utree;

import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

/**
 * Note that UtopianTree4 is a partial function.
 * It is not defined for negative cycNumber.
 *
 * @author zlatozar@gmail.com
 */
public class UtopianTree4 {

    interface Cycles {
        int iterator(int from, int to, int acc);
    }

    private static int getLengthAfterNCycles(final int cycNumber) {

        final Cycles cycles = new Cycles() {

            @Override
            public int iterator(int from, int to, int acc) {

                if (from == to) {
                    return acc;
                }

                if (from % 2 == 0) {
                    acc = acc + 1;

                } else {
                    acc = acc * 2;
                }

                // now tail recursive!
                return iterator(from + 1, to, acc);
            }
        };

        return cycles.iterator(0, cycNumber, 1);
    }

    public static void main(String[] args) {

        final Scanner scanner = new Scanner(System.in);
        final int testCases = scanner.nextInt();

        for (int i = 0; i < testCases; i++) {
            System.out.println(getLengthAfterNCycles(scanner.nextInt()));
        }
    }
}
