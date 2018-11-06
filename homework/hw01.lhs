---
title: Homework 01: Basic IO actions
date: September 2018
---

The solution for Homework 1 will eventually be available [here](../code/Hw01.hs). 

You will write a very simle program, that asks the user his/her name and
birth year and tells him/her age in the year 2030. An example
execution for this program would be:

     | What is your name? Maria
     | What is your birth year? 1992
     | Hi Maria
     | In 2030, you will be: 38

Try executing your program and providing a non number entry to the
question for the birth year. For example:

     | What is your name? Maria
     | What is your birth year? hello
     | Hi Maria
     | In 2030, you will be: *** Exception: Prelude.read: no parse

We will later see more resilient ways to rewrite your code, in order
catch and treat errors. 
