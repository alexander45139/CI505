# Logfile parsing 

## *Something has gone terribly wrong!*


We're really not sure what happened, but we did manage to recover the
log file [`error.log`](error.log). It seems to consist of a different log
message on each line. Each line begins with a character indicating the
type of log message it represents:


+ `I` for informational messages,
+ `W` for warnings, and
+ `E` for errors.


The error message lines then have an integer indicating the severity
of the error, with 1 being the sort of error you might get around to
caring about sometime next summer, and 100 being epic, catastrophic
failure. 

All log messages then have an integer timestamp followed by textual
content that runs to the end of the line. Here is a snippet of the log
file including an informational message followed by a level 2 error
message:

```
I 147 mice in the air, I’m afraid, but you might catch a bat, and
E 2 148 #56k istereadeat lod 200ff] BOOTMEM
```


It's all quite confusing; clearly we need a program to sort through this mess. We've come up
with some datatypes to capture the structure of this logfile format:

```Haskell
data MessageType = Info | Warning | Error Int deriving (Show, Eq)
type TimeStamp   = Int
data LogMessage  = LogMessage MessageType TimeStamp String deriving (Show, Eq)
```

We've provided you with a module `Log.hs` containing these datatype declarations, along
with some other useful functions. Enter your solutions in the `Main.hs` file, the first few lines of which looks like this:

```Haskell
module Main where

import System.IO
import Log
```

which sets up your file as a module named `Main`, and imports the
`System.IO` and `Log` modules so you can use the types and functions they
provides. 

Use the [Prelude](https://www.haskell.org/ghc/docs/latest/html/libraries/haskell2010-1.0.0.0/Prelude.html)
functions to make your solution as concise, high-level, and
functional as possible. Functions which may (or may not) be useful to you include
`lines`, `words`, `unwords`, `take`, `drop`, `(.)`,
`map`, `filter` and `fold`.

## Exercise 1

The first thing we need to do is to read the log file. Define a function

```Haskell
readLogFile :: String -> IO [String]
```

that takes the path to a log file, reads its contents and returns a list of strings in which
each element contains one line from the log file. Test your work with the function
`testReadLogFile`, provided in `Log.hs`. `testReadLogFile` takes your
`readLogFile` function, a `String` containing the path to the log file, and
simply prints the contents of the list of strings. So, its output should match the original
file. Use `sample.log` for testing (you could also use `error.log` but it is
quite a large file).

## Exercise 2

The next step is figuring out how to parse an individual message. However, perhaps the file is
even more corrupted than we thought: maybe individual lines are garbled. So, we can't be sure
that a line from the input will be a valid `LogMessage`. Thus, we define a type (included
in the provided `Log.hs`) to allow for the possibility of failure:

```Haskell
type MaybeLogMessage = ValidLM LogMessage | InvalidLM String deriving (Show, Eq)
```


As you can see, a `MaybeLogMessage` either contains a proper `LogMessage` or just
an unformatted string. Now, you can define a function

```Haskell
parseMessage:: String -> MaybeLogMessage
```

which parses an individual line from the logfile. For example, 

```Haskell
parseMessage "E 2 562 helphelp" 
    == ValidLM (LogMessage (Error 2) 562 "helphelp")
parseMessage "I 29 lalala" 
    == ValidLM(LogMessage Info 29 "lalala")
parseMessage "This is not in the right format" 
    == InvalidLM "This is not in the right format"
```

Use the `read` function to convert `String`s to `Int`s. Test your work
with the function `testParseMessage`, which takes as arguments your `readLogFile`
function, the path to a log file, and your `parseMessage` function.

## Exercise 3

It isn't terribly hard to make `parseMessage` work over all the lines in a file. But,
doing so would produce a list of `MaybeLogMessage`s, where we really just want a
list of `LogMessage`s for further processing -- let's throw out the invalid messages. Write a
function

```Haskell
validMessagesOnly :: [MaybeLogMessage] -> [LogMessage]
```

that throws out invalid messages.

## Exercise 4

Now, we can put these pieces together to define

```Haskell
parse :: String -> IO [LogMessage]
```
 
which parses an entire log file at once and returns its contents as a
list of `LogMessage`s. To test your function, use the `testParse`
function provided in the `Log` module, giving it as arguments your
parse function, the number of messages to parse, and the log file to
parse from (which should also be in the same folder as your
assignment). For example, after loading your assignment into GHCi,
type something like this at the prompt:

```Haskell
testParse parse 10 "error.log"
```

## Putting the logs in order

Unfortunately, due to the error messages being generated by multiple
servers in multiple locations around the globe, a lightning storm, a
failed disk, and a bored yet incompetent programmer, the log messages
are horribly out of order. Until we do some organising, there will be
no way to make sense of what went wrong.

## Exercise 5

Any sorting function is going to need to compare two `LogMessage`s to see which one
should come first. But, since we've just created the `LogMessage` type, there is no way
for the computer to know how to compare two of them. We must write a comparison function! In
general, comparing two items for ordering can yield one of three results: less-than, equal-to,
or greater-than. Haskell codifies this idea as a datatype

```Haskell
data Ordering = LT | EQ | GT
```

`Ordering` is part of the `Prelude` (the set of things automatically included),
so its definition doesn't appear in `Log.hs` nor should it appear in your code. Define a
function

```haskell
compareMsgs :: LogMessage -> LogMessage -> Ordering
```

that compares two `LogMessage`s based on their timestamps. Here are some examples:

```
compareMsgs (LogMessage Warning 153 "Not a speck of light is showing, so the danger must be
growing...") (LogMessage Info 208 "the Weighted Companion Cube cannot talk") 
  == LT
compareMsgs (LogMessage (Error 101) 2001 "My God! It's full of stars!") (LogMessage Info 2001
"Daisy, Daisy, give me your answer do.") 
  == EQ
```

## Exercise 6

Now that you have said how to compare messages, you can sort the list. Write a function

```haskell
sortMessages :: [LogMessage] -> [LogMessage]
```

that sorts the list of messages. Do not write out a full sorting algorithm! Instead, poke
around in the [`Data.List`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-List.html)
module looking for a function that will help you.


## Logfile postmortem

## Exercise 7

Now that we can sort the log messages, the only thing left to do is extract the relevant
information. We have decided that "relevant" means "errors with a severity of at least
50". Write a function

```haskell
whatWentWrong :: [LogMessage] -> [(TimeStamp, String)]
```

which takes an unsorted list of `LogMessage`s and returns a list of the messages
corresponding to any errors with a severity of 50 or greater, sorted by timestamp. (Of course,
you can use your functions from the previous exercises to do the sorting.) For example, suppose
our logfile looked like this:

```
I 6 Completed armadillo processing
I 1 Nothing to report
E 99 10 Flange failed!
I 4 Everything normal
I 11 Initiating self-destruct sequence
E 70 3 Way too many pickles
E 65 8 Bad pickle-flange interaction detected
W 5 Flange is due for a check-up
I 7 Out for lunch, back in two timesteps
E 20 2 Too many pickles
I 9 Back from lunch 
```

This file is provided as `sample.log`. There are four errors, three of which have a
severity of greater than 50. The output of `whatWentWrong` on `sample.log` ought
to be

```haskell
[(3,"Way too many pickles"),(8,"Bad pickle-flange interaction detected"),(10,"Flange failed!")]
```

You can test your `whatWentWrong` function with `testWhatWentWrong`, which is
also provided by the `Log` module. You should provide `testWhatWentWrong` with your parse
function, your `whatWentWrong` function, and the name of the logfile to parse.

## Exercise 8

Having read log files, parsed individual messages, sorted them and detected messages with
certain properties, we now need to present the results to our boss. So, we will write the
results of our analysis to a file. This file will present a sorted list of the most severe
incidents, showing just the time at which an incident occurred and the error message. Define
the function

```haskell
processLogFile :: String -> String -> IO ()
```

which does the following:


+ read the log file at the path given by the first `String` argument,
+ parse the log file into a list of messages,
+ transform that list into a sorted list of error messages with a severity of at least 50,
+ write this sorted list to a file whose name is given by the second `String`
  argument. Each row in the file should have the form `[timestamp] message`. For
  example,

```
[3] Way too many pickles
[8] Bad pickle-flange interaction detected
[10] Flange failed!
```
