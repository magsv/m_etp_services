m_etp_services
==============

Background
--------------
The following repository is a **early** test bed for the Energistics specification for next generation of data transfer protocol (ETP). It has been developed as part of testing the early specification and focuses on utilising other technologies than plain Java, #C or JavaScript. In addition erlang is very well suited for this setup with its stability and performance characteristics and has a nice fit to be used on e.g. embedded small devices.

Development status
-------------------
The current library is a **early** test bed implementation and focuses on testing out the underlying technology as part of the current [Energistics ETP](http://www.energistics.org/standards-portfolio/energistics-transfer-protocol) specification which utilises websockets and Apache Avro to defined the data transfer protocol.   
The server test implementation is done in **Erlang** which is a language which is very well suited for this challenge. To demonstrate the language neutrality of the specification a python client has been used to test the Erlang server implementation.  
The server is currently implemented with cowboy as a front end and utilising mnesia as the backing session storage. For each connection a fsm is spawned keeping track of the different states as visible as part of the ETP specification.  
For the moment no errors from the backend is propagated back to the calling client as this has yet not been defined in the specification as well as how to handle session survailability in case of e.g. a network glitch. In case of a breakdown or that the calling client suddenly ends the websocket communication, the actual session will survive as part of the mnesia storage backend (utilising in memory and disk storage).  
Note also that the erlang code is currently bloated with lager statements in order to make it easier to follow what is happening behind the scenes for people not beeing aware of how to utilise the **Erlang** tracing facilities.

The current status of the package is that it supports the following

1. Supports request session and opensession procedure including close session
2. Currently the data handling protocols have not been implemented
3. Some of the protocols will fail to load, this is a bug that will be fixed
4. The project includes schemas that have been flattened in order to support parsing as either the avro erlang, c++ or python libraries like nested schemas
5. The current test bed is tailored at running on a single node which will change sooner or later 

Prerequisites
---------------
1. Erlang 17*
2. Rebar
3. Python 2.7 to run test clients, python will also need the **websocket-client** library as well as the **Apache Avro** python libraries

The current library can be built following the instructions below or the supplied dockerfile can be used to create a **Docker** image with everything preinstalled ready to be tested following the docker instructions below.

Setup with a clean install on localhost
================

The procedure below is based on that you will not perform an release of the application but rather run it directly on a single node.

Python client setup
-------------------------

The unit test client and python client library requires the python **websocket-client** library together with the **Apache Avro** python libraries. To install them using pip run ->  **pip install websocket-client**, **pip install avro**
In order to test the python sample code you would need to edit the testUtils.py file and update the it with the correct schema paths..


Building the Erlang code
------------------------

1. Run **make deps** -> this will pull the required dependencies 
2. Run **make** -> this will build the Erlang dependencies and the application itself

Setup mnesia
----------------------

1. Make sure that the **MNESIA_DIR** as specified in the make file exists
2. Run **make m_etp_server** -> this will start the application and mnesia
3. Open a new terminal windowd and run **make erlconnect** -> this will connect you to the running erlang node
4. Enter **mnesia:stop()** -> this will stop mnesia in order to create the base schema
5. Enter **mnesia:create_schema([node()]).** -> this will create the required base schema on the current node (we are only running this on one node)
6. Enter **mnesia:start()** -> this will start mnesia again
7. Enter **m_etp_store_init_tables:create_tables([node()]).** -> this will create the required tables in mnesia and the operation should return ok
8. You are now ready to load some data....
9. Run **make load_protocols** -> this will load the avro required schemas

Setup using Docker - first time
====================

1. Run **make docker_prepare** -> this will copy files to be used in the container
2. Run **make docker_build_image** -> this will create a new image with Ubuntu 14.04 and all of the required software including this library.
3. Run **make docker_run_bash** to connect to the bash of the newly created image
4. To create the Mnesia schemas follow the **Setup mnesia** part
5. Open up a localhost browser instance and enter http://localhost:8080/ping -> this should give you a response back from the erlang server...
6. To clean up temp files used with docker run **make docker_clean_prepare**

Setup using Docker -- after image has been build
------------------------

1. Run **make docker_run_bash** to connect to the bash of the image
2. Run **make m_etp_server** to start the server

Possible issues with Docker
------------------------

If Docker complains about that the port is already in user upon connecting through bash, make sure that you stop all running containers with:

**sudo docker stop --time=10 $(docker ps -a -q)**

Inspecting the Erlang node
==========================

You can at any time connect to the running node and inspect the status of the processes and Mnesia by using **make erlconnect**. Note though that you will need to update the **NODE_NAME** variable in the make file prior to this.
After you have run make erlconnect you should be connected to the running node and just use **observer:start().** to open up the observer console to inspect mnesia tables and running processes.

Services
=========================

1. http://localhost:8080/ping -> ping plain http test
2. http://localhost:8080/m_etp_socket_ping -> ping websocket test
3. http://localhost:8080/m_etp_socket_service -> ETP socket acceptor to use protocola






