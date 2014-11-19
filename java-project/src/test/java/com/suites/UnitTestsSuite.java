package com.suites;

import java.io.File;

import org.junit.runners.Suite;
import org.junit.runners.model.InitializationError;
import org.junit.runners.model.RunnerBuilder;

import com.suites.annotation.UnitTest;
import com.suites.util.ClasspathClassesFinder;

/**
 * Suite that will search for all classes annotated by @UnitTest or nothing
 * 
 * @author Zlatozar Zhelyazkov <zlatozar@gmail.com>
 */
public class UnitTestsSuite extends Suite {

	private static final String sep = File.separator;

	public UnitTestsSuite(Class<?> clazz, RunnerBuilder builder) throws InitializationError {
		this(builder, clazz, ClasspathClassesFinder.getSuiteClasses("com", UnitTest.class, true,
				sep + "integration" + sep));
	}

	public UnitTestsSuite(RunnerBuilder builder, Class<?> clazz, Class<?>[] suiteClasses)
			throws InitializationError {

		super(builder, clazz, suiteClasses);
	}

}
