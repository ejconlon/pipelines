pipelines
=========

A utility for running workflows.  See `examples/simple.json` for a simple plan definition.

To watch and execute that plan:

    stack build && stack exec -- stack exec pipelines-exe -- --base=/tmp/pipelines --plan=examples/simple.json &
    touch /tmp/pipelines/simple/inputs/hello.txt
    cat /tmp/pipelines/simple/executions/hello/state.json
    fg

TODO
----
* Capture and log stdin/stdout
* Watch multiple plans
* Parallelize execution
* Support resource aquisition
* Expand tests
