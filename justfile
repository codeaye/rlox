alias br := showcase
alias d := debug

set quiet := true

debug:
    cargo run -- test.sel
showcase:
    just build && time ./target/release/selena test.sel
build:
    cargo build --release --quiet > log
run:
    ./target/release/selena test.sel
flame:
    cargo build --profile profiling
    samply record ./target/profiling/selena ./test.sel