/**
 * Copyright 2012, Cisco Systems, Inc. All Rights Reserved.
 *
 * This is UNPUBLISHED PROPRIETARY SOURCE CODE of Cisco Systems, Inc; the
 * contents of this file may not be disclosed to third parties, copied or
 * duplicated in any form, in whole or in part, without the prior written
 * permission of Cisco Systems, Inc.
 *
 * RESTRICTED RIGHTS LEGEND: Use, duplication or disclosure by the Government is
 * subject to restrictions as set forth in subdivision (c)(1)(ii) of the Rights
 * in Technical Data and Computer Software clause at DFARS 252.227-7013, and/or
 * in similar or successor clauses in the FAR, DOD or NASA FAR Supplement.
 * Unpublished - rights reserved under the Copyright Laws of the United States.
 */
package com.suites.annotation;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

/**
 * Annotation used to define a test as an Unit Test.
 * 
 * @author Zlatozar Zhelyazkov <zzhelyaz@cisco.com>
 */
@Retention(RetentionPolicy.RUNTIME)
public @interface UnitTest {

}
