alias br := showcase
alias d := debug
alias db := debugv
alias b := bacon

set quiet := true

debug:
    cargo run -- ./test.sel 0
debugv:
    cargo run -- ./test.sel 0
bacon:
    bacon
showcase:
    just build && time ./target/release/selena test.sel
build:
    cargo build --release
run:
    ./target/release/selena test.sel
flame:
    cargo build --profile profiling
    samply record ./target/profiling/selena ./test.sel