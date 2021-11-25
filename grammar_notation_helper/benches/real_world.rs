use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("real world", |b| {
        b.iter(|| {
            grammar_notation_helper::generate(black_box(include_str!(
                "../../jssat_frontend_js/src/ast/parse_nodes.json"
            )))
        })
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
