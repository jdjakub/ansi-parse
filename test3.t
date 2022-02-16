Use `test3.exe -readable` to convert test.txt into readable ANSI escape sequences
  $ ./test3.exe -readable < test.txt
  make: Leaving directory '/pinata-ci/repos/pinata/v1/cmd/pitfall'
  time="2016-07-04T13:27:08Z" level=info msg="Stopping VM..." 
  time="2016-07-04T13:27:10Z" level=info msg="Reverting to Snapshot..." 
  time="2016-07-04T13:27:20Z" level=info msg="Starting VM..." 
  time="2016-07-04T13:27:25Z" level=info msg="Waiting for IP Address..." 
  time="2016-07-04T13:30:28Z" level=info msg="IP Address: 172.16.10.190" 
  time="2016-07-04T13:30:35Z" level=info msg="Connected to 172.16.10.190:22" 
  time="2016-07-04T13:30:35Z" level=info msg="Uploading Test Dir to server" 
  time="2016-07-04T13:30:57Z" level=info msg="Attempting to install Docker for Mac" 
  time="2016-07-04T13:31:30Z" level=info msg="Setting bashrc..." 
  time="2016-07-04T13:31:30Z" level=info msg="Running the tests..." 
  time="2016-07-04T13:31:30Z" level=info msg="Command: ./rt-local -l nostart,installer -vvv -x run" 
  STDERR| [reset()][style(1;39)][INFO  ][reset()] Start
  STDERR| [reset()][style(1;39)][INFO  ][reset()] Entering Group pinata in cases
  STDERR| [reset()][style(1;39)][INFO  ][reset()] pinata::ginit()
  STDERR| [reset()][style(1;37)][DEBUG ][reset()] Executing: ['/bin/sh', '-x', 'group.sh', 'init'] in cases
  STDERR| [reset()][style(1;31)][STDERR][reset()] + set -x
  STDERR| [reset()][style(1;31)][STDERR][reset()] + . /Users/docker/pinata/cases/_lib/lib.sh
  STDERR| [reset()][style(1;31)][STDERR][reset()] ++ '[' -f /Users/docker/pinata/lib/lib.sh ']'
  STDERR| [reset()][style(1;31)][STDERR][reset()] ++ . /Users/docker/pinata/lib/lib.sh
  STDERR| [reset()][style(1;31)][STDERR][reset()] +++ RT_TEST_CANCEL=253
