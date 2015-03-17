package com.hackerrank;

import org.junit.Assert;
import org.junit.Rule;
import org.junit.Test;
import org.junit.contrib.java.lang.system.TextFromStandardInputStream;

import java.util.List;

import static org.junit.contrib.java.lang.system.TextFromStandardInputStream.emptyStandardInputStream;

/**
 * Tests for {@link com.hackerrank.HackerRankTemplate}
 */
public class HackerRankTemplateTest {

    private HackerRankTemplate solution = new HackerRankTemplate();

    @Rule
    public TextFromStandardInputStream systemInMock = emptyStandardInputStream();

    @Test
    public void shouldTakeCommandLineParameters() {

        systemInMock.provideText("First\nSecond\n");
        List<String> passedStrings = solution.takeArguments();

        // verify arguments
    }

}
