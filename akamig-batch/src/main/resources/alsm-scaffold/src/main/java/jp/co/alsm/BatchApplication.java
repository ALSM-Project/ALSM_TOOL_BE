package jp.co.alsm;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.springframework.batch.core.BatchStatus;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.JobParametersBuilder;
import org.springframework.batch.core.configuration.annotation.EnableBatchProcessing;
import org.springframework.batch.core.launch.JobLauncher;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.ExitCodeGenerator;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.PropertySource;

@SpringBootApplication(scanBasePackages = {"jp.co.alsm"})
@EnableBatchProcessing
@PropertySource(value = {"classpath:Config.properties", "file:Config.properties"}, ignoreResourceNotFound = true)
public class BatchApplication implements CommandLineRunner, ExitCodeGenerator {

  @Autowired
  private JobLauncher jobLauncher;

  @Autowired
  private ApplicationContext context;

  private int exitCode;

  public static void main(String[] args) {
    System.exit(SpringApplication.exit(SpringApplication.run(BatchApplication.class, args)));
  }

  @Override
  public void run(String... args) throws Exception {
    Map<String, Job> jobs = new TreeMap<>(context.getBeansOfType(Job.class));
    if (jobs.isEmpty()) {
      System.err.println("No Spring Batch job was found under the scanned packages " + scanned() + ".");
      exitCode = 1;
      return;
    }
    String requested = firstJobArgument(args);
    String name = requested == null ? jobs.keySet().iterator().next() : findJob(jobs, requested);
    if (name == null) {
      System.err.println("There is no job named \"" + requested + "\". Jobs you can run: " + jobs.keySet());
      exitCode = 2;
      return;
    }
    if (requested == null && jobs.size() > 1) {
      System.out.println("No job name given, running " + name + ". Jobs you can run: " + jobs.keySet());
    }
    // NAME=value arguments become job parameters: the converted programs read the path of each
    // file from the parameter named after its DD (java -jar app.jar CBACT02C CARDFILE=cards.txt).
    JobParametersBuilder parameters = new JobParametersBuilder().addLong("run.id", System.currentTimeMillis());
    for (String arg : args) {
      int eq = arg.indexOf('=');
      if (!arg.startsWith("-") && eq > 0) {
        parameters.addString(arg.substring(0, eq), arg.substring(eq + 1));
      }
    }
    JobExecution execution = jobLauncher.run(jobs.get(name), parameters.toJobParameters());
    System.out.println("Job " + name + " finished with status " + execution.getStatus());
    exitCode = execution.getStatus() == BatchStatus.COMPLETED ? 0 : 1;
  }

  @Override
  public int getExitCode() {
    return exitCode;
  }

  /** The job name is the first argument that is neither a Spring option (--server.port=0) nor NAME=value. */
  private static String firstJobArgument(String... args) {
    for (String arg : args) {
      if (!arg.startsWith("-") && arg.indexOf('=') < 0) {
        return arg;
      }
    }
    return null;
  }

  /** Exact bean name first, then ignoring case, so "model01" finds the job "Model01". */
  private static String findJob(Map<String, Job> jobs, String requested) {
    if (jobs.containsKey(requested)) {
      return requested;
    }
    for (String name : jobs.keySet()) {
      if (name.equalsIgnoreCase(requested)) {
        return name;
      }
    }
    return null;
  }

  private static List<String> scanned() {
    List<String> packages = new ArrayList<>();
    for (String p : BatchApplication.class.getAnnotation(SpringBootApplication.class).scanBasePackages()) {
      packages.add(p);
    }
    return packages;
  }
}
