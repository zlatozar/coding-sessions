package com.suites.annotation;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

/**
 * Annotation used to define a test as a Integration Test.
 * 
 * @author Zlatozar Zhelyazkov <zlatozar@gmail.com>
 */
@Retention(RetentionPolicy.RUNTIME)
public @interface IntegrationTest {

}
