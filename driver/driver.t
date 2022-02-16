Use driver.exe to convert test.txt into HTML
  $ ./driver.exe < ../test.txt
  <pre><span style="; ; ; ; ">make: Leaving directory '/pinata-ci/repos/pinata/v1/cmd/pitfall'
  time=&quot;2016-07-04T13:27:08Z&quot; level=info msg=&quot;Stopping VM...&quot; 
  time=&quot;2016-07-04T13:27:10Z&quot; level=info msg=&quot;Reverting to Snapshot...&quot; 
  time=&quot;2016-07-04T13:27:20Z&quot; level=info msg=&quot;Starting VM...&quot; 
  time=&quot;2016-07-04T13:27:25Z&quot; level=info msg=&quot;Waiting for IP Address...&quot; 
  time=&quot;2016-07-04T13:30:28Z&quot; level=info msg=&quot;IP Address: 172.16.10.190&quot; 
  time=&quot;2016-07-04T13:30:35Z&quot; level=info msg=&quot;Connected to 172.16.10.190:22&quot; 
  time=&quot;2016-07-04T13:30:35Z&quot; level=info msg=&quot;Uploading Test Dir to server&quot; 
  time=&quot;2016-07-04T13:30:57Z&quot; level=info msg=&quot;Attempting to install Docker for Mac&quot; 
  time=&quot;2016-07-04T13:31:30Z&quot; level=info msg=&quot;Setting bashrc...&quot; 
  time=&quot;2016-07-04T13:31:30Z&quot; level=info msg=&quot;Running the tests...&quot; 
  time=&quot;2016-07-04T13:31:30Z&quot; level=info msg=&quot;Command: ./rt-local -l nostart,installer -vvv -x run&quot; 
  STDERR| <span style="font-weight: bold; ; ; ; ">[INFO  ]</span> Start
  STDERR| <span style="font-weight: bold; ; ; ; ">[INFO  ]</span> Entering Group pinata in cases
  STDERR| <span style="font-weight: bold; ; ; ; ">[INFO  ]</span> pinata::ginit()
  STDERR| <span style="font-weight: bold; ; ; color: white; ">[DEBUG ]</span> Executing: ['/bin/sh', '-x', 'group.sh', 'init'] in cases
  STDERR| <span style="font-weight: bold; ; ; color: red; ">[STDERR]</span> + set -x
  STDERR| <span style="font-weight: bold; ; ; color: red; ">[STDERR]</span> + . /Users/docker/pinata/cases/_lib/lib.sh
  STDERR| <span style="font-weight: bold; ; ; color: red; ">[STDERR]</span> ++ '[' -f /Users/docker/pinata/lib/lib.sh ']'
  STDERR| <span style="font-weight: bold; ; ; color: red; ">[STDERR]</span> ++ . /Users/docker/pinata/lib/lib.sh
  STDERR| <span style="font-weight: bold; ; ; color: red; ">[STDERR]</span> +++ RT_TEST_CANCEL=253
  </span></pre>
