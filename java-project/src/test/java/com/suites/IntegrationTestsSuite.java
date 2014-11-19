package com.suites;

import java.io.File;

import org.junit.runners.Suite;
import org.junit.runners.model.InitializationError;
import org.junit.runners.model.RunnerBuilder;

import com.suites.annotation.IntegrationTest;
import com.suites.util.ClasspathClassesFinder;

/**
 * Suite that will search for all classes (in 'com' package) annotated
 * by @IntegrationTest.
 * 
 * @author Zlatozar Zhelyazkov <zlatozar@gmail.com>
 */
public class IntegrationTestsSuite extends Suite {

	private static final String sep = File.separator;

	public IntegrationTestsSuite(Class<?> clazz, RunnerBuilder builder) throws InitializationError {
		this(builder, clazz, ClasspathClassesFinder.getSuiteClasses("com", IntegrationTest.class,
				false, sep + "unit" + sep));
	}

	public IntegrationTestsSuite(RunnerBuilder builder, Class<?> clazz, Class<?>[] suiteClasses)
			throws InitializationError {

		super(builder, clazz, suiteClasses);
	}

}
