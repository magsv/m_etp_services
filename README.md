m_etp_services
==============


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

