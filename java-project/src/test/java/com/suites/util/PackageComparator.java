package com.suites.util;

import java.util.Comparator;

/**
 * Comparator used to order Unit and Integration tests. <br/>
 * Correct order is:
 * 
 * <pre>
 * com.pre.*
 * com.zlatozar.*
 * "unspecified" (all other)
 * com.post.*
 * </pre>
 * 
 * Equal package names are compared lexicographically.
 * 
 * @author Zlatozar Zhelyazkov <zlatozar@gmail.com>
 * 
 */
public class PackageComparator implements Comparator<Class<?>> {

	public int compare(Class<?> firstClass, Class<?> secondClass) {

		final String firstClassName = firstClass.getName();
		final String secondClassName = secondClass.getName();

		// ___________________________________________________________________
		// insert "com.pre" as first parameter (higher priority)

		if (firstClassName.startsWith("com.pre") && secondClassName.startsWith("com.pre")) {
			return -(firstClass.getName().compareTo(secondClassName));
		}

		if (firstClassName.startsWith("com.pre") && secondClassName.startsWith("com.zlatozar")) {
			return 1;
		}

		if (firstClassName.startsWith("com.pre") && secondClassName.startsWith("com.post")) {
			return 1;
		}

		// for all other
		if (firstClassName.startsWith("com.pre")) {
			return 1;
		}

		// for all other
		if (firstClassName.startsWith("com.liferay")) {
			return 1;
		}

		// ___________________________________________________________________
		// insert "com.zlatozar" as first parameter

		if (firstClassName.startsWith("com.zlatozar") && secondClassName.startsWith("com.zlatozar")) {
			return -(firstClass.getName().compareTo(secondClassName));
		}

		if (firstClassName.startsWith("com.zlatozar") && secondClassName.startsWith("com.pre")) {
			return -1;
		}

		if (firstClassName.startsWith("com.zlatozar") && secondClassName.startsWith("com.post")) {
			return 1;
		}

		// for all other
		if (firstClassName.startsWith("com.zlatozar")) {
			return 1;
		}

		// ___________________________________________________________________
		// insert "not specified" as first parameter

		if (isUnspecified(firstClassName) && isUnspecified(secondClassName)) {
			return -(firstClass.getName().compareTo(secondClassName));
		}

		if (isUnspecified(firstClassName) && secondClassName.startsWith("com.pre")) {
			return -1;
		}

		if (isUnspecified(firstClassName) && secondClassName.startsWith("com.zlatozar")) {
			return -1;
		}

		if (isUnspecified(firstClassName) && secondClassName.startsWith("com.post")) {
			return 1;
		}

		// ___________________________________________________________________
		// insert "com.post" as first parameter (lower priority)

		if (firstClassName.startsWith("com.post") && secondClassName.startsWith("com.post")) {
			return -(firstClass.getName().compareTo(secondClassName));
		}

		if (firstClassName.startsWith("com.post") && secondClassName.startsWith("com.pre")) {
			return -1;
		}

		if (firstClassName.startsWith("com.post") && secondClassName.startsWith("com.zlatozar")) {
			return -1;
		}

		// for all other
		if (firstClassName.startsWith("com.post")) {
			return -1;
		}

		// ___________________________________________________________________
		// default (compare two strings lexicographically)

		return firstClass.getName().compareTo(secondClassName);
	}

	// -----------------------------------------------------------------------
	// Helper method
	// -----------------------------------------------------------------------

	private boolean isUnspecified(final String className) {
		if (className.startsWith("com.pre") || className.startsWith("com.zlatozar")
				|| className.startsWith("com.post")) {

			return false;
		}

		return true;
	}
}
