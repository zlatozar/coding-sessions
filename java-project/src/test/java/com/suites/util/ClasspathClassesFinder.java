package com.suites.util;

import java.io.File;
import java.io.IOException;
import java.lang.annotation.Annotation;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;
import java.util.List;
import java.util.Stack;

/**
 * Utility class that will search, in the Java CLASSPATH, all classes annotated
 * with a given annotation (@UnitTest or @IntegrationTest) and that belongs to
 * a given package name. We pass package name to narrow the searching scope.
 * 
 * @author Zlatozar Zhelyazkov <zlatozar@gmail.com>
 */
public final class ClasspathClassesFinder {

	private static final String sep = File.separator;

	// Packages that contain framework classes (or disabled tests)
	private final static Stack<String> BANNED_DIRS = new Stack<String>();

	static {
		// TODO: Place it in resource file

		BANNED_DIRS.add(sep + "mock" + sep);
		BANNED_DIRS.add(sep + "dummy" + sep);
		BANNED_DIRS.add(sep + "suites" + sep);
		BANNED_DIRS.add("TestCase");
		BANNED_DIRS.add("TestBase");
	}

	/**
	 * Get the list of classes of a given package name, and that are annotated
	 * by a given annotation.
	 * 
	 * @param packageName
	 *            The package name of the classes.
	 * @param testAnnotation
	 *            The annotation the class should be annotated with.
	 * @param additionalRestr
	 *            Additional directories that should be banned
	 * @return The List of classes that matches the requirements.
	 */
	public static Class<?>[] getSuiteClasses(String packageName,
			Class<? extends Annotation> testAnnotation, boolean allowEmpty,
			String... additionalRestr) {

		try {
			for (String restr : additionalRestr) {
				BANNED_DIRS.add(restr);
			}

			return trace(prePostReorder(getClasses(packageName, testAnnotation, allowEmpty)),
					testAnnotation.getName());

		} catch (Exception e) {
			e.printStackTrace();

		} finally {
			for (int i = 0; i < additionalRestr.length; i++) {
				BANNED_DIRS.pop();
			}
		}

		return null;
	}

	// -----------------------------------------------------------------------
	// Helper methods
	// -----------------------------------------------------------------------

	private static Class<?>[] prePostReorder(final List<Class<?>> collectedClasses) {

		Collections.sort(collectedClasses, new PackageComparator());
		Collections.reverse(collectedClasses);

		return collectedClasses.toArray(new Class[collectedClasses.size()]);
	}

	/**
	 * Get the list of classes of a given package name, and that are annotated
	 * by a given annotation.
	 * 
	 * @param packageName
	 *            The package name of the classes.
	 * @param annotation
	 *            The annotation the class should be annotated with.
	 * @return The List of classes that matches the requirements.
	 * 
	 * @throws ClassNotFoundException
	 * @throws IOException
	 */
	private static List<Class<?>> getClasses(String packageName,
			Class<? extends Annotation> annotation, boolean allowEmpty)
			throws ClassNotFoundException, IOException {

		ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
		String path = packageName.replace('.', sep.charAt(0));

		// Get whole CLASSPATH
		Enumeration<URL> resources = classLoader.getResources(path);
		List<File> dirs = new ArrayList<File>();
		while (resources.hasMoreElements()) {
			URL resource = resources.nextElement();

			// load only test classes
			if (resource.getFile().contains("test-classes")) {
				dirs.add(new File(resource.getFile()));
			}
		}

		ArrayList<Class<?>> classes = new ArrayList<Class<?>>();
		for (File directory : dirs) {
			classes.addAll(findClasses(directory, packageName, annotation, allowEmpty));
		}

		return classes;
	}

	/**
	 * Find classes, in a given directory (recursively), for a given package
	 * name, that are annotated by a given annotation.
	 * 
	 * IMPORTANT: Unit tests should avoid direct execution annotations. If they
	 * should be counted as specific test better extend particular *BaseTestCase
	 * class.
	 * 
	 * @param directory
	 *            The directory where to look for.
	 * @param packageName
	 *            The package name of the classes.
	 * @param annotation
	 *            The annotation the class should be annotated with.
	 * @return The List of classes that matches the requirements.
	 * 
	 * @throws ClassNotFoundException
	 */
	private static List<Class<?>> findClasses(File directory, String packageName,
			Class<? extends Annotation> annotation, boolean allowEmpty)
			throws ClassNotFoundException {

		List<Class<?>> classes = new ArrayList<Class<?>>();
		if (!directory.exists()) {
			return Collections.emptyList();
		}

		File[] files = directory.listFiles();
		for (File file : files) {

			if (file.isDirectory()) {
				classes.addAll(findClasses(file, packageName + "." + file.getName(), annotation,
						allowEmpty));

			} else if (file.getName().endsWith(".class")) {

				if (isDirBanned(file.getAbsolutePath())) {
					continue;
				}

				// System.out.println("Loading: " + file.getAbsolutePath());

				// Remove the .class at the end of the filename
				Class<?> clazz = Class.forName(packageName + '.'
						+ file.getName().substring(0, file.getName().length() - 6));

				if (clazz.getSuperclass() != null) {

					// directly annotated
					if (clazz.getAnnotation(annotation) != null) {
						classes.add(clazz);
						continue;
					}

					// let's check it's super - *BaseTestCase
					if (clazz.getSuperclass().getAnnotation(annotation) != null) {
						classes.add(clazz);
						continue;
					}

					// skip if other than given annotation
					if (clazz.getSuperclass().getAnnotations().length > 0) {
						continue;
					}
				}

				// Is it a regular JUnit test with/without annotations?
				if (clazz.getName().endsWith("Test") && allowEmpty) {
					classes.add(clazz);
					continue;
				}
			}
		}

		return Collections.unmodifiableList(classes);
	}

	/**
	 * Checks if directory is banned for a given file
	 * 
	 * @param fileName
	 * @return if class could be collected
	 */
	private static boolean isDirBanned(String fileName) {
		for (String banned : BANNED_DIRS) {
			if (fileName.contains(banned)) {
				// System.out.println("Skip: " + fileName);
				return true;
			}
		}
		return false;
	}

	/**
	 * Proxy method that show names of loaded tests
	 * 
	 * @param collected
	 * @return the same collection
	 */
	private static Class<?>[] trace(Class<?>[] collected, String annotation) {
	
		if (collected.length > 0) {
			System.out.println("\n" + collected.length + " collected tests for: " + annotation
					+ "\n");
			for (Class<?> test : collected) {
				System.out.println(test.getName());
			}
			System.out.println("\n");
		}

		return collected;
	}

}
