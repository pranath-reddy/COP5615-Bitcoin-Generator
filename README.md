# Distributed Bitcoin Mining

Developed as part of coursework for COP5615 - Distributed Operating System Principles  
  
**Programming Language:** Erlang

## Description

The purpose of this project is to create a distributed bitcoin mining system using Erlang and the Actor Model. In the context of this project, mining a coin has a different definition. Each bitcoin has a specific number of leading zeroes and is based on the SHA256 hash function. Finding a set of strings (bitcoins) that, when hashed using SHA-256, contain the required number of zeros as the prefix is the stated problem. The user's username is predefined as the first character of the string.

## Overview

![Block Diagram](https://github.com/pranath-reddy/COP5615-Bitcoin-Generator/blob/main/DOSP%20Project-1.png)

> Block diagram of the distributed implementation

## Execution

**Starting a server:** ```server:main(W, K)``` 
* *W - No of coins to mine (W/n in case of distributed),*
* *K - No of leading zeros*
  
**Starting a client:** ```client:main(ID, Address)``` 
* *ID - Unique ID to identify the client,* 
* *Address - Address of the server*
