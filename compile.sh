make bin/coqtop && \
make init && \
bin/coqtop < input.v > stdout && \
tail -n +18 stdout > test.rs && \
rustc --crate-type=lib test.rs
