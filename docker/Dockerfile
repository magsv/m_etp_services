FROM 		ubuntu:14.04

# Update Base System

RUN 		apt-get update
RUN 		apt-get -y upgrade

# Set Environment Variables

ENV 		HOME /root
# Set Language Environment

RUN 		apt-get install -y language-pack-en

ENV 		LANGUAGE en_US.UTF-8
ENV 		LANG en_US.UTF-8
ENV 		LC_ALL en_US.UTF-8
RUN 		locale-gen en_US.UTF-8
RUN 		dpkg-reconfigure locales
ENV 		DEBIAN_FRONTEND noninteractive

# Install Basic Packages

RUN 		apt-get install -y build-essential software-properties-common

RUN 		apt-get install -y wget curl git man unzip screen


# Install Erlang
WORKDIR 	/tmp

RUN 		wget http://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb

RUN 		dpkg -i erlang-solutions_1.0_all.deb 

RUN 		apt-get update 

RUN 		apt-get install -y erlang incron

RUN 		apt-get clean

RUN 		rm -rf /tmp/*

RUN 		mkdir /source

#Install Python

RUN 		apt-get install -y python 

#Install PIP
RUN 		apt-get install -y python-pip

#Install websocket-client
RUN 	    pip install websocket-client

#Install Apache Avro Python libs
RUN 	    pip install avro

## Install Rebar
WORKDIR 	/tmp
RUN git clone git://github.com/rebar/rebar.git
RUN cd /tmp/rebar && ./bootstrap
RUN mkdir /tools
RUN cp /tmp/rebar/rebar /tools
RUN ln -s /tools/rebar /usr/local/bin/rebar
RUN rm -rf /tmp/*

##Install m_etp_server source
RUN mkdir /development
WORKDIR /development


