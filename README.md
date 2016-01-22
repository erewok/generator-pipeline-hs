## Generator Pipeline Consumer Project

If you have a `generator` this will consume its output and print stats at the end.

### To Install

This project has been built with `stack` so, run `stack install`.

### To Run

The exectuble expects two arguments: location of the generator executable and a number of records to process. Here's an example with the generator exec in a parent directory:

```sh
$ generator-pipeline-exe ../generator 100
```

### Goals and Motivations

This project is closely related to a family of problems that I have been interested in recently, namely calculating streaming values in linear time. (I have found the Conduit library to be a great resource for this stuff.) Last year, I experimented with [using Conduit to calculate moving averages](https://gist.github.com/pellagic-puffbomb/9239cb57834789886f5c). 

For this project, initially it seemed like it would be a cool idea to spit out live-processed results (current results as well as things like moving averages) and serve them on static page (just some HTML and JS) that live updates in pseudo 'real time'. Thus, I wrote it with that in mind.

However, I realized that I probably wouldn't have the time to figure out how to set up a socket server that a socket-io program could subscribe to.

I settled on simply doing the processing and printing to STDOUT.


### Some Profiling Stats

Some profiling stats are included. The basic processing algorithm is roughly linear to the size of the input stream. 

For memory profiling (see [./generator-pipeline-exe.ps](./generator-pipeline-exe.ps)) the program was run to consume 100,000 results.

For time profiling (see [./generator-pipeline-exe.prof](./generator-pipeline-exe.prof)), the program was run to consume 1,000,000 results. (It seems like parsing rows takes a surprising amount of time...).

### How long did this take?

Probably about 2.5 days total of coding, broken up over two weeks, but it was fun and interesting.

### Problems and ToDos

- A number of things should use the type system but do not, [index-based results for instance](./src/Lib.hs#L40).

- The are [`error` calls](result column for each type of result, and max/min are a simple comparison.), which is the worst kind of shortcut. With more time, those would be eliminated first.

- The [Max/Min algorithms are incorrect](./src/Accumulators/Basic.hs#L83). They compare a `0` on the left with any number on the right. If that first number or later numbers are 0, those results will be overwritten. This needs to be fixed.
