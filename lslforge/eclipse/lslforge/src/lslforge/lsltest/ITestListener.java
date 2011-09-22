package lslforge.lsltest;

public interface ITestListener {

    void newTestResult(TestResult result);

    void testLaunched(int numTests);
    void testFinished();
}
