# Usage of the REST-API
## Setup
To run the server from source, you simply need to run the following line in your terminal:
```bash
lein run -a
```
Conexp-clj's server will per default start on port `8080`, which will be used in later examples. To choose your own port use:
```bash
lein run -a -p YOUR_PORT
```
To stop the server call the following function whereas `ms` refers to the time in ms until the server shuts down. In this time only requests send prior to the function call will be processed.
```clj
(stop-server ms)
```
This function is the only Clojure syntax you need to know.

### Hot reload
Another option is to start the server in development mode which supports hot reloading and returns the output with more human readable formatting:

```
lein run -d
```
You can choose a port in the same way as above.

## Basic requests
Communication with the server is done with JSON POST requests. It's assumed you already know your way with JSON, otherwise you can pick up whatever is suitable to your language of choice here https://json.org/. You could also use `curl` right from your terminal.

From here on out we only give documentation for everything inside the body of the POST request.

Each request can have one individual ID and any number of functions and data of your choosing only limited by the size limit of POST requests. A simple working request could look like this:
```JSON
{
  "id" : "my request",
  "addition" : {"type" : "function",
                "name" : "+",
                "args" : ["num1", "num2"]},
  "num1" : {"type" : "integer",
            "data" : 1},
  "num2" : {"type" : "integer",
            "data" : 2}
}
```
Which will result in:
```JSON
{
 "id" : "my request",
 "addition" : {"status" : 0,
               "type"   : "long",
               "result" : 3,
               "msg"    : null}
}
```
Now there's a lot to unpack here:

- Your ID can be anything and will be copied to your response untouched.

- Both functions and data are represented by JSON objects with their ID as name. The one thing both of them share is the `type` attribute which will always be `function` for an function and can be anything else for data.
- If your object is a function you have to provide two more arguments: `name` and `args`. The actual function you want to call is `name` and `args` is a JSON list with the needed arguments for your function. You cannot write the data as argument but must instead use the ID of data objects or even other functions - which must be defiend above the current function to be used as argument. Later on there is a segment about all accepted functions.
- Data objects only take one additional argument which is `data`. `type` describes the structure of your data which we will later explain in detail with all accepted arguments. `data` then provides the input which usally is a nested list.
- The response contains all IDs used for functions. `status` returns an integer to describe if the function could be successfully run, for now just note that `0` means success. `result` as the name describes contains whatever your choosen function returns with the given arguments. `msg` contains potential error messages and `type` describes the data. 

### Nested Requests
As noted above you can use other functions as argument as long as they are defined above your function. It doesn't matter where you place your data objects. An example how this could like is the following:

```JSON
{                                                                               
  "id" : "my request",                                                             
  "addition" : {"type" : "function",                                               
                "name" : "+",                                                      
                "args" : ["num1", "num2"]},                                        
  "num1" : {"type" : "integer",                                                    
            "data" : 1},                                                           
  "num2" : {"type" : "integer",                                                    
            "data" : 2},                                                           
  "subtraction" : {"type" : "function",                                            
                   "name" : "-",                                                   
                   "args" : ["addition", "num2"]}                                  
} 
```
Which yields:
```JSON
{
  "id" : "my request",
  "addition" : {"status" : 0,
                "result" : 3},
  "subtraction" : {"status" : 0,
                   "result" : 1}
}
```
### Muting functions
There's a special type for functions to not include them in the response, e.g. when their result is only used as input for other functions. This type is `silent_function`.

## Accepted functions
All functions found in the [API documentation](API) are usable with the REST-API. If you're not coming from Clojure just note how to read the function signatures:
```clj
([a b c][a b])
```
Each function can have multiple signatures. Clojure describes this with a list of all possible signatures, so given the example you could either use the arguments `a`, `b` and `c` or just `a` and `b`. The same pattern applies to functions with only one possible signature.

Additionaly there are some shorthands and basic functions usable which are listed below. Any other function will throw an exception.

### Additional functions
- `count`

### Shorthands
Shorthands are a way to simplify common nested function calls and can be added under `conexp-clj/src/main/clojure/conexp/api/shorthands.clj`.
Have a look at this:
```Clojure
(function-a value-a (function-b value-a (function-c value-a value-b)))
```
Using the API as explained one would need to create several function and data objects. To reduce those the following shorthand could be used:
```Clojure
(defn shorthand-abc [value-a value-b]
  (function-a value-a (function-b value-a (function-c value-a value-b))))
```

## Accepted data types
All following data types describe both how the API expects certain constructs as well as how it will return those structures in the response.

Besides the standard JSON constructs, namely `integer`, `string`, `boolean`, `null`, `list` and `object`, the following types were added: `map`, `context`, `context_file`, `mv_context`, `mv_context_file`, `lattice`, `implication`, `implication_set` and `layout`.

### Map
`map` type data is a normal JSON object. It is required when a functions calls for a hash-map and changes formatting on server side. What follows is an example of a map which assigns values to edges:

```JSON
{
  "type" : "map",
  "data" : {"[a, b]" : 5,
            "[a, c]" : 8,
            "[b, c]" : 3}
 }
```
Note that keys can only be of `string` type.

### Context
Each `context` type object has to have three attributes: `objects`, `attributes` and `incidence`. The former two being only lists of your respective objects and attributes while the last one is a list of pairs.
```
   | x y z 
---+-------
 a | x . .
 b | . x .
 c | . . x
```
```JSON
{
  "type" : "context",
  "data" : {"objects"    : ["a", "b", "c"],
            "attributes" : ["x", "y", "z"],
            "incidence"  : [["a", "x"], ["b", "y"], ["c", "z"]]}
}
```

### Context file
`context_file` allows to send a whole context as string in any file format recognised by conexp-clj. Some more details about those formats can be found [here](Common-FCA-File-Formats-for-Formal-Contexts).

### Many valued context

As with `context`, a `mv_context` type has to have `objects`, `attributes` and `incidence`. The difference being that the `incidence` is a JSON object which maps a pair as key to their value.

```
   | x y 
---+-----
 a | 3 1
 b | 5 5
```
```JSON
{
  "type" : "context",
  "data" : {"objects"    : ["a", "b"],
            "attributes" : ["x", "y"],
            "incidence"  : {"[a, x]" : 3, 
                            "[b, x]" : 5, 
                            "[b, y]" : 5, 
                            "[a, y]" : 1}}
}
```

Note how each there has to be a value for each combination in the incidence.

### Many valued context file

`mv_context_file` allows to send a whole context in any recognised file format.

### Lattice
`lattice` type objects consist of `nodes` and `edges`, respectively being a list and a list of pairs.

```
   d
 /   \
b     c
 \   /
   a
```
```JSON
{
  "type" : "lattice",
  "data" : {"nodes" : ["a", "b", "c", "d"],
            "edges" : [["a", "b"], ["a", "c"], ["b", "d"], ["c", "d"], ["a", "d"],
                       ["a", "a"], ["b", "b"], ["c", "c"], ["d", "d"]]}
}
```
Note how transitive and reflective edges are required.

### Implication

Each `implication` has one premise and one conclusion, represented by a nested list.

```
#{"a"} -> #{"b", "c"}
```
```JSON
{
  "type" : "implication",
  "data" : [["a"],["b", "c"]]
}
```

### Implication set
`implication_set` is almost the same as `implication`, but puts multiple implications in yet another list.

```
#{"a"} -> #{"b", "c"}
#{"d"} -> #{"b"}
```
```JSON
{
  "type" : "implication_set",
  "data" : [[["a"],["b", "c"]],
            [["d"],["b"]]]
}
```

### Layout
The `layout` type gives in addition to a `lattice` information needed to draw the lattice, namely `positions`, `connections`, `upper-labels` and `lower-labels`. All of them, except for `connections` being a list, are objects which map nodes to their respective vales.
```
      d_{label4}
   /    \ 
 /        \
b^{label2} c_{label3}
 \        / 
   \    / 
      a^{label1}
```
```JSON
{
  "type" : "layout",
  "data" : {"lattice"      : {"nodes" : ["a", "b", "c", "d"],
                              "edges" : [["a", "b"], ["a", "c"], ["b", "d"], 
                                         ["c", "d"], ["a", "d"], ["a", "a"], 
                                         ["b", "b"], ["c", "c"], ["d", "d"]]}},
            "positions"    : {"a" : [0, 0],
                              "b" : [-1, 1],
                              "c" : [1, 1],
                              "d" : [0, 2]},
            "connections"  : [["a", "b"], ["a", "c"], ["b", "d"], ["c", "d"]],
            "upper-labels" : {"a" : ["label1", [0.25, 0.25]], 
                              "b" : ["label2", null],
                              "c" : [null, null],
                              "d" : [null, null]},
            "lower-label"  : {"a" : [null, null], 
                              "b" : [null, null],
                              "c" : ["label3", null],
                              "d" : ["label4", [0.25, 1.75]]}
}
```
Note how connections now only include visible edges.

Labels are optional, but if you include them, there must be an entry for each node with a pair of label and position. Both values can be `null`.

## Response
The response uses all the same data types from above but with one added feature, the `type` shows nested types.

Consider this list of formal concepts:

```JSON
[[["a", "b"],["d"]],
 [["c"], ["e"]]]
```
Each concept is a list with two lists of their own, the intent and extend. Which makes for three lists over all. Since JSON dosen't differentiate between different kinds of collections in Clojure, except for maps, the following `type` may return:

```JSON
"type" : "string_set_set_set"
```

### Response status
Each response has a `status`. There are currently three values `status` can take based on http status codes:
- `200` everything went fine
- `204` the return value is `null` despite no error  
- `400` errors occured