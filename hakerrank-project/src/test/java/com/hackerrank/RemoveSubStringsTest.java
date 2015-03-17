package com.hackerrank;

import org.junit.Assert;
import org.junit.Rule;
import org.junit.Test;
import org.junit.contrib.java.lang.system.TextFromStandardInputStream;

import java.util.List;

import static org.junit.contrib.java.lang.system.TextFromStandardInputStream.emptyStandardInputStream;

/**
 * Tests for {@link com.hackerrank.RemoveSubStrings}
 */
public class RemoveSubStringsTest{

    private RemoveSubStrings removeSubStrings = new RemoveSubStrings();

    @Rule
    public TextFromStandardInputStream systemInMock = emptyStandardInputStream();

    @Test
    public void shouldTakeCommandLineParameters() {
        systemInMock.provideText("ccdaabcdbb\n2 ab cd\n");
        List<String> passedStrings = removeSubStrings.takeArguments();
        Assert.assertEquals(3, passedStrings.size());

        Assert.assertEquals("ccdaabcdbb", passedStrings.get(0));
        Assert.assertEquals("ab", passedStrings.get(1));
        Assert.assertEquals("cd", passedStrings.get(2));
    }

    @Test
    public void shouldReturnWholeStingIfSubStringsAreWithBiggerLength() {
        systemInMock.provideText("b\n2 ab cd\n");
        List<String> cmdArgs = removeSubStrings.takeArguments();

        String longString = cmdArgs.get(0);
        List<String> subStr = cmdArgs.subList(1, cmdArgs.size());

        Assert.assertEquals("b", removeSubStrings.reduceString(longString, subStr));
    }
}
