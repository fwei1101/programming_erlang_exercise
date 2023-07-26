# In shell
erl
1> c(hello).
{ok,hello}
2> hello:start().
Hello world
ok
3> halt().

# Out shell
erlc hello.erl
erl -noshell -s hello start -s init stop
Hello world
