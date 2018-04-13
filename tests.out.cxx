/* Generated file, do not edit */

#ifndef CXXTEST_RUNNING
#define CXXTEST_RUNNING
#endif

#define _CXXTEST_HAVE_STD
#include <cxxtest/TestListener.h>
#include <cxxtest/TestTracker.h>
#include <cxxtest/TestRunner.h>
#include <cxxtest/RealDescriptions.h>
#include <cxxtest/TestMain.h>
#include <cxxtest/ErrorPrinter.h>

int main( int argc, char *argv[] ) {
 int status;
    CxxTest::ErrorPrinter tmp;
    CxxTest::RealWorldDescription::_worldName = "cxxtest";
    status = CxxTest::Main< CxxTest::ErrorPrinter >( tmp, argc, argv );
    return status;
}
bool suite_Tests_init = false;
#include "tests.cxx"

static Tests suite_Tests;

static CxxTest::List Tests_Tests = { 0, 0 };
CxxTest::StaticSuiteDescription suiteDescription_Tests( "tests.cxx", 4, "Tests", suite_Tests, Tests_Tests );

static class TestDescription_suite_Tests_testIntern : public CxxTest::RealTestDescription {
public:
 TestDescription_suite_Tests_testIntern() : CxxTest::RealTestDescription( Tests_Tests, suiteDescription_Tests, 6, "testIntern" ) {}
 void runTest() { suite_Tests.testIntern(); }
} testDescription_suite_Tests_testIntern;

static class TestDescription_suite_Tests_test2Intern : public CxxTest::RealTestDescription {
public:
 TestDescription_suite_Tests_test2Intern() : CxxTest::RealTestDescription( Tests_Tests, suiteDescription_Tests, 74, "test2Intern" ) {}
 void runTest() { suite_Tests.test2Intern(); }
} testDescription_suite_Tests_test2Intern;

static class TestDescription_suite_Tests_testBoolean : public CxxTest::RealTestDescription {
public:
 TestDescription_suite_Tests_testBoolean() : CxxTest::RealTestDescription( Tests_Tests, suiteDescription_Tests, 96, "testBoolean" ) {}
 void runTest() { suite_Tests.testBoolean(); }
} testDescription_suite_Tests_testBoolean;

static class TestDescription_suite_Tests_testDouble : public CxxTest::RealTestDescription {
public:
 TestDescription_suite_Tests_testDouble() : CxxTest::RealTestDescription( Tests_Tests, suiteDescription_Tests, 109, "testDouble" ) {}
 void runTest() { suite_Tests.testDouble(); }
} testDescription_suite_Tests_testDouble;

static class TestDescription_suite_Tests_testQuote : public CxxTest::RealTestDescription {
public:
 TestDescription_suite_Tests_testQuote() : CxxTest::RealTestDescription( Tests_Tests, suiteDescription_Tests, 131, "testQuote" ) {}
 void runTest() { suite_Tests.testQuote(); }
} testDescription_suite_Tests_testQuote;

static class TestDescription_suite_Tests_testVararg : public CxxTest::RealTestDescription {
public:
 TestDescription_suite_Tests_testVararg() : CxxTest::RealTestDescription( Tests_Tests, suiteDescription_Tests, 166, "testVararg" ) {}
 void runTest() { suite_Tests.testVararg(); }
} testDescription_suite_Tests_testVararg;

#include <cxxtest/Root.cpp>
const char* CxxTest::RealWorldDescription::_worldName = "cxxtest";
