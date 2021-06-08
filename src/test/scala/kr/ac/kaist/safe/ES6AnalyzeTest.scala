/**
 * *****************************************************************************
 * Copyright (c) 2016-2019, KAIST.
 * All rights reserved.
 *
 * Use is subject to license terms.
 *
 * This distribution may include materials developed by third parties.
 * ****************************************************************************
 */

package kr.ac.kaist.safe

class ES6AnalyzeTest extends AnalyzeTest {
  // registration
  val ES6TestDir = testDir + "ES6Tests"
  analyzeHelper("ES6Analyze", List(ES6TestDir))
}
