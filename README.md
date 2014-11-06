m_etp_services
==============

Background
--------------
The following repository is a test bed for the Energistics specification for next generation of data transfer protocol (ETP). It has been developed as part of testing the early specification and focuses on utilising other technologies than plain Java, #C or JavaScript. 

Development status
-------------------
The current library is a early test bed implementation and focuses on testing out the underlying technology as part of the current Energistics ETP specification which utilises websockets and Apache Avro to defined the data transfer protocol.   
The server test implementation is done in Erlang which is a language which is very well suited for this challenge. To demonstrate the language neutrality of the specification a python client has been used to test the Erlang server implementation.  
The server is currently implemented with cowboy as a front end and utilising mnesia as the backing session storage. For each connection a fsm is spawned keeping track of the different states as visible as part of the ETP specification.  
For the moment no errors from the backend is propagated back to the calling client as this has yet not been defined in the specification as well as how to handle session survailability in case of e.g. a network glitch. In case of a breakdown or that the calling client suddenly ends the websocket communication, the actual session will survive as part of the mnesia storage backend (utilising in memory and disk storage).

Python client requirements
==============

The unit test client and python client library requires the python websocket-client
library. To install it use e.g. "pip install websocket-client"

Prerequisites
=================
1. Erlang 17*
2. Rebar
3. Python to run test clients, python will also need the **websocket-client** library as well as the **Apache Avro** python libraries

Setup
================

The procedure below is based on that you will not perform an release of the application but rather run it directly on a single node.

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

