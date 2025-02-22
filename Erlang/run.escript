#!/usr/bin/env escript

main(_) ->
    compile_files(),
    run_program().

compile_files() ->
    compile:file(file_reader),
    compile:file(main).

run_program() ->
    main:start().
