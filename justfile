alias br := showcase
alias d := debug
alias b := bacon

set quiet := true

debug:
    cargo run -- ./test.sel
bacon:
    bacon
showcase:
    just build && time ./target/release/selena test.sel
build:
    cargo build --release --quiet > log
run:
    ./target/release/selena test.sel
flame:
    cargo build --profile profiling
    samply record ./target/profiling/selena ./test.sel